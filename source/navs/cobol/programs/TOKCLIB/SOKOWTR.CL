/. ***********************************************************  ./
/. *    倉庫側　ＳＴＲＷＴＲ　再投入　　　　　　　　         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    ?NWKSTN    :=       @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)
                                      /.##片岡配送センター##./
    IF         ?WKSTN    = 'WKSTNH6A'
        THEN   STRWTR PRINTH6A,XXPKATQ,@NO
               STRWTR PRINTX6A,XXPKATQL,@NO
               STRWTR PRINTHKA,XXPKATQ1,@NO
    END
                                      /.##フバサミ配送センター##./
    IF         ?WKSTN    = 'WKSTNH63'
        THEN   STRWTR PRINTH63,XXPFUBQ,@NO
               STRWTR DEV-PRINTHFB,SPLQ-XXPFUBQL,TERM-@NO
               STRWTR DEV-PRINTH6F,SPLQ-XXPFUB2Q,TERM-@NO
    END
                                      /.##フバサミ配送センター##./
    IF         ?WKSTN    = 'WKSTNHFB'
        THEN   STRWTR PRINTH63,XXPFUBQ,@NO
               STRWTR DEV-PRINTHFB,SPLQ-XXPFUBQL,TERM-@NO
               STRWTR DEV-PRINTH6F,SPLQ-XXPFUB2Q,TERM-@NO
    END
                                      /.##フバサミ配送センター##./
    IF         ?WKSTN    = 'WKSTNH6F'
        THEN   STRWTR PRINTH63,XXPFUBQ,@NO
               STRWTR DEV-PRINTHFB,SPLQ-XXPFUBQL,TERM-@NO
               STRWTR DEV-PRINTH6F,SPLQ-XXPFUB2Q,TERM-@NO
    END
                                      /.##大和倉庫配送センター##./
    IF         ?WKSTN    = 'WKSTNH60'
        THEN   STRWTR PRINTH60,XXPDIWQ,@NO
               STRWTR PRINTH6L,XXPDIWLQ,@NO
    END
                                      /.##カトーレック岡山##./
    IF         ?WKSTN    = 'WKSTNHT5'
        THEN   STRWTR PRINTHT5,XXPKARQ,@NO
               STRWTR PRINTHTL,XXPKARQL,@NO
               STRWTR PRINTHKR,XXPKARQP,@NO
    END
                                      /.##富岡配送センター##./
    IF         ?WKSTN    = 'WKSTNH83'
        THEN   STRWTR PRINTH83,XXPTOMQ,@NO
               STRWTR PRINTX83,XXPXLX83,@NO
    END
                                      /.##鴻巣配送センター##./
    IF         ?WKSTN    = 'WKSTNH84'
        THEN   STRWTR PRINTH84,XXPKOUQ,@NO
               STRWTR PRINTHKO,XXPKOUQL,@NO
    END
                                      /.##手綱園芸##./
    IF         ?WKSTN    = 'WKSTNH90'
        THEN   STRWTR PRINTH90,XXPTEZQ,@NO
               STRWTR PRINTHTZ,XXPTEZQL,@NO
    END
                                      /.##西尾植物_##./
    IF         ?WKSTN    = 'WKSTNH86'
        THEN   STRWTR PRINTH86,XXPNISQ1,@NO
               STRWTR PRINTH8A,XXPNISQ2,@NO
               STRWTR PRINTHNS,XXPNISQL,@NO
    END
                                      /.##西尾植物_##./
    IF         ?WKSTN    = 'WKSTNH8A'
        THEN   STRWTR PRINTH86,XXPNISQ1,@NO
               STRWTR PRINTH8A,XXPNISQ2,@NO
               STRWTR PRINTHNS,XXPNISQL,@NO
    END
                                      /.##北上配送##./
    IF         ?WKSTN    = 'WKSTNHE2'
        THEN   STRWTR PRINTHE2,XXPHITQ,@NO
               STRWTR PRINTHES,XXPHITQ2,@NO
               STRWTR PRINTHKT,XXPHITQL,@NO
    END
                                      /.##北上配送##./
    IF         ?WKSTN    = 'WKSTNHKT'
        THEN   STRWTR PRINTHE2,XXPHITQ,@NO
               STRWTR PRINTHES,XXPHITQ2,@NO
               STRWTR PRINTHKT,XXPHITQL,@NO
    END
                                      /.##仙台営業所##./
    IF         ?WKSTN    = 'WKSTNH42'
        THEN   STRWTR PRINTH42,XXPSENQ,@NO
    END
                                      /.##蔦井倉庫##./
    IF         ?WKSTN    = 'WKSTNHT9'
        THEN   STRWTR PRINTHT9,XXPTUTQ,@NO
    END
                                      /.##北海道支店##./
    IF         ?WKSTN    = 'WKSTNH45'
        THEN   STRWTR PRINTH45,XXPHOKQ,@NO
    END
                                      /.##花の海##./
    IF         ?WKSTN    = 'WKSTNHT4'
        THEN   STRWTR PRINTHT4,XXPHANQ,@NO
               STRWTR PRINTHHL,XXPHANQL,@NO
    END
                                      /.##長野ｾﾙﾄｯﾌﾟ##./
    IF         ?WKSTN    = 'WKSTNH22'
        THEN   STRWTR PRINTH22,XXPNAGQ,@NO
    END
                                      /.##秋田園芸資材##./
    IF         ?WKSTN    = 'WKSTNH6S'
        THEN   STRWTR PRINTH6S,XXPAKIQ,@NO
    END
                                      /.##岡山総合花卉##./
    IF         ?WKSTN    = 'WKSTNH53'
        THEN   STRWTR PRINTH53,XXPOKSQ,@NO
               STRWTR PRINTHOL,XXPOKSQL,@NO
    END
                                      /.##片岡配送追加##./
    IF         ?WKSTN    = 'WKSTNH6B'
        THEN   STRWTR PRINTH6A,XXPKATQ,@NO
               STRWTR PRINTX6A,XXPKATQL,@NO
    END
                                      /.##日栄愛知##./
    IF         ?WKSTN    = 'WKSTNH6V'
        THEN   STRWTR DEV-PRINTH6V,SPLQ-XXPNTAQ,TERM-@NO
               STRWTR DEV-PRINTHNA,SPLQ-XXPNTAQL,TERM-@NO
    END
                                      /.##さくら配送１##./
    IF         ?WKSTN    = 'WKSTNHS1'
        THEN   STRWTR PRINTHS1,XXPSKRQ1,@NO
               STRWTR PRINTHS2,XXPSKRQ2,@NO
               STRWTR PRINTHSL,XXPSKRQL,@NO
    END
                                      /.##さくら配送２##./
    IF         ?WKSTN    = 'WKSTNHS2'
        THEN   STRWTR PRINTHS1,XXPSKRQ1,@NO
               STRWTR PRINTHS2,XXPSKRQ2,@NO
               STRWTR PRINTHSL,XXPSKRQL,@NO
    END
                                      /.##新札幌配送　##./
    IF         ?WKSTN    = 'WKSTNHTU'
        THEN   STRWTR PRINTHAW,XXPAWSQ,@NO
               STRWTR PRINTHAL,XXPAWSQL,@NO
    END
                                      /.##カトーレック岡山##./
    IF         ?WKSTN    = 'WKSTNHKR'
        THEN   STRWTR PRINTHT5,XXPKARQ,@NO
               STRWTR PRINTHTL,XXPKARQL,@NO
               STRWTR PRINTHKR,XXPKARQP,@NO
    END
                                      /.##ＦＬＫ九州##./
    IF         ?WKSTN    = 'WKSTNHFL'
        THEN   STRWTR PRINTHFL,XXPFLKQL,@NO
    END

    RETURN    PGMEC-@PGMEC
