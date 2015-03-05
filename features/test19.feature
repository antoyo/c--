Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./main.native tests/test19.c`
        Then the output should contain exactly:
        """
        1, 2
        42, 24, 12
        3, 4, 5
        6, 7, 8, 9
        42
        24
        0, 5
        1, 4
        2, 3

        """
