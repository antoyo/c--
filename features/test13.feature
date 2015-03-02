Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./src/c-- tests/test13.c`
        Then the output should contain exactly:
        """
        result == 0
        result == 8
        result == 15
        result == 25
        result == 2
        c == 6
        c == 7
        c == 8
        c == 9
        c == 4
        c == 3
        c == 13
        c == 15
        c == 12
        c == 36
        c == 18
        c == 3

        """
