Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./src/c-- tests/test16.c`
        Then the output should contain exactly:
        """
        16
        -10
        -5
        -3
        15
        -5
        3
        yes
        -576

        """
