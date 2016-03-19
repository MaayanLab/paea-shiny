from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException, TimeoutException
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
import unittest
import os

URL = os.environ['VIEWER_URL']
TESTS_PATH = os.path.abspath(os.path.dirname(__file__))


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

    def test_can_upload_expression_data(self):
        """ Test if we can upload expression data
        """
        self.browser.get(URL)

        wait = WebDriverWait(self.browser, 10)

        (
            wait.until(
                expected_conditions.visibility_of_element_located((
                    By.ID,
                    'datain'
                )))
            .send_keys(os.path.join(TESTS_PATH, 'data/example_expression_data.csv'))
        )

        wait.until(expected_conditions.visibility_of_element_located((
            By.ID,
            'sampleclass'
        )))

        self.assertTrue(
            self.browser.find_elements_by_xpath("//td[text()='NR2C2AP']"),
            "We should see preview table"
        )

        self.browser.find_element_by_link_text('Plots').click()

        try:
            wait.until(expected_conditions.visibility_of_element_located((
                By.TAG_NAME,
                'svg'
            )))

        except TimeoutException:
            self.fail('We should see density plot')

    def tearDown(self):
        self.browser.quit()


if __name__ == '__main__':
    unittest.main()
