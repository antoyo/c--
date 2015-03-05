Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./main.native tests/test15.c`
        Then the output should contain exactly:
        """
        3.141592
        1000.000000
        10000.000000

        """
