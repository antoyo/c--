Feature: Output

    In order to specify expected output
    As a developer using Cucumber
    I want to use the "the output should contain" step

    Scenario: Listing home directory
        When I run `./main.native tests/test9.c`
        Then the output should contain exactly:
        """
        while
        while
        while
        while
        while
        do while
        do while
        do while
        do while
        do while
        do while
        do while
        do while
        do while
        do while
        while
        while
        while
        while
        while
        for
        for
        for

        """
