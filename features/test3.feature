Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./main.native tests/test3.c`
        Then the output should contain exactly:
        """
        12
        42
        24
        25
        10
        -12

        """
