Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./src/c-- tests/test6.c`
        Then the output should contain exactly:
        """
        24
        42
        50
        -50

        """
