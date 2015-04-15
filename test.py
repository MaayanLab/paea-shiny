from selenium import webdriver
import unittest
import os

URL = os.environ['VIEWER_URL']


class BasicWorkflowTest(unittest.TestCase):

    def setUp(self):
        self.browser = webdriver.Firefox()
        self.browser.implicitly_wait(2)

    def test_can_load(self):
        self.browser.get(URL)
        self.assertEqual(
            'NASB Microtask Viewer',
            self.browser.title,
            'We should see page title'
        )

    def test_can_load_chdir(self):
        self.browser.get(URL)
        (
            self.browser
                .find_element_by_id('workflow_panel')
                .find_element_by_link_text('Characteristic Direction Analysis')
                .click()
        )

        self.assertTrue(
            'Upload your dataset' in self.browser
            .find_element_by_id('run_chdir_help')
            .text,
            'We should see chdir run message'
        )

        self.assertTrue(
            'No data' in self.browser
            .find_element_by_id('chdir_downloads_message')
            .text,
            'We should see chdir downloads message'
        )


    def tearDown(self):
        self.browser.quit()


if __name__ == '__main__':
    unittest.main()
