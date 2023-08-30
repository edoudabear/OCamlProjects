let quit_loop = ref false in
    while not !quit_loop do
        print_string "Have you had enough yet? (y/n) ";
        print_string "Really Really ? ";
        let str = read_line () in
        if str.[0] = 'y' then quit_loop := true
    done;;