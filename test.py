from selenium import webdriver
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
import unittest
import os

URL = os.environ['VIEWER_URL']


class BasicWorkflowTest(unittest.TestCase):

    def setUp(self):
        self.browser = webdriver.Firefox()

    def test_can_load(self):
        """ Test if page loads up to the point
        when we can access title
        """
        self.browser.get(URL)
        self.assertEqual(
            'NASB Microtask Viewer',
            self.browser.title,
            'We should see page title'
        )

    def test_can_load_chdir_tab(self):
        """ Test if we can access chdir tab
        and read help messages
        """
        self.browser.get(URL)
        (
            self.browser
                .find_element_by_id('workflow_panel')
                .find_element_by_link_text('Characteristic Direction Analysis')
                .click()
        )

        wait = WebDriverWait(self.browser, 10)

        self.assertTrue(
            'Upload your dataset' in wait.until(
                expected_conditions.visibility_of_element_located((
                    By.ID,
                    'run_chdir_help'
                ))).text,
            'We should see chdir run message'
        )

        self.assertTrue(
            'No data' in wait.until(
                expected_conditions.visibility_of_element_located((
                    By.ID,
                    'chdir_downloads_message'
                ))).text,
            'We should see chdir downloads message'
        )

    def test_can_load_paea_tab(self):
        """ Test if we can access paea tab
        and read help messages
        """
        self.browser.get(URL)
        (
            self.browser
                .find_element_by_id('workflow_panel')
                .find_element_by_link_text('Principle Angle Enrichment Analysis')
                .click()
        )

        wait = WebDriverWait(self.browser, 10)

        self.assertTrue(
            'Before you can' in wait.until(
                expected_conditions.visibility_of_element_located((
                    By.ID,
                    'run_paea_message'
                ))).text,
            'We should see paea run message'
        )



    def tearDown(self):
        self.browser.quit()


if __name__ == '__main__':
    unittest.main()
