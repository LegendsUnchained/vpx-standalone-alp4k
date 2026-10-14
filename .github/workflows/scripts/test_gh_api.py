import io
import json
import unittest
import urllib.error
from unittest.mock import patch

import gh_api


def http_error(code):
    return urllib.error.HTTPError("https://uploads.github.com/x", code, "err", {}, io.BytesIO(b"{}"))


class Response(io.BytesIO):
    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


class UploadRetryTest(unittest.TestCase):
    def api(self, outcomes, leftovers=()):
        """An Api whose uploads go through outcomes in turn: an exception to
        raise, or a dict to answer with. leftovers are the assets a failed
        upload left in the release."""
        api = gh_api.Api("o/r", "t")
        self.posts, self.deleted = [], []
        outcomes = list(outcomes)

        def request(method, url, data=None, **kw):
            self.posts.append(url)
            out = outcomes.pop(0)
            if isinstance(out, Exception):
                raise out
            return Response(json.dumps(out).encode())

        api._request = request
        api.assets = lambda release_id: list(leftovers)
        api.delete_asset = self.deleted.append
        return api

    @patch("gh_api.time.sleep")
    def test_a_502_is_retried_after_deleting_the_half_upload(self, sleep):
        api = self.api([http_error(502), {"id": 9, "name": "vpx-a.zip"}],
                       leftovers=[{"id": 4, "name": "vpx-a.zip", "state": "starter"},
                                  {"id": 5, "name": "vpx-b.zip"}])
        self.assertEqual(api.upload(1, "vpx-a.zip", b"zip")["id"], 9)
        self.assertEqual(len(self.posts), 2)
        self.assertEqual(self.deleted, [4])
        sleep.assert_called_once_with(gh_api.UPLOAD_RETRIES[0])

    @patch("gh_api.time.sleep")
    def test_a_dropped_connection_is_retried(self, sleep):
        api = self.api([urllib.error.URLError("reset"), TimeoutError(), {"id": 9}])
        self.assertEqual(api.upload(1, "vpx-a.zip", b"zip")["id"], 9)
        self.assertEqual(len(self.posts), 3)

    @patch("gh_api.time.sleep")
    def test_a_client_error_is_not_retried(self, sleep):
        api = self.api([http_error(422)])
        with self.assertRaises(urllib.error.HTTPError):
            api.upload(1, "vpx-a.zip", b"zip")
        self.assertEqual(len(self.posts), 1)
        sleep.assert_not_called()

    @patch("gh_api.time.sleep")
    def test_it_gives_up_after_the_last_retry(self, sleep):
        api = self.api([http_error(502)] * (len(gh_api.UPLOAD_RETRIES) + 1))
        with self.assertRaises(urllib.error.HTTPError):
            api.upload(1, "vpx-a.zip", b"zip")
        self.assertEqual(len(self.posts), len(gh_api.UPLOAD_RETRIES) + 1)


if __name__ == "__main__":
    unittest.main()
