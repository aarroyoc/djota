:- initialization((
    logtalk_load(lgtunit(loader)),
    logtalk_load(test, [hook(lgtunit)]),
    test::run
)).