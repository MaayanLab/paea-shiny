from selenium import webdriver
import unittest
import os

URL = os.environ['VIEWER_URL']

class BasicWorkflowTest(unittest.TestCase):

    def setUp(self):
        self.browser = webdriver.Firefox()


    def test_can_load(self):
        self.browser.get(URL)
        self.assertEqual(
            'NASB Microtask Viewer',
            self.browser.title
        )

    def tearDown(self):
        self.browser.quit()


if __name__ == '__main__':
    unittest.main()
