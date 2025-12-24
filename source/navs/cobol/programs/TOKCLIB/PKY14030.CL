/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    オフライン                           *  ./
/. *   JOB-ID      :    PKY14030                             *  ./
/. *   JOB-NAME    :    スポット出荷入力                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PKY14030'
    VAR       ?STEP     ,STRING*8
/.##ﾌｧｲﾙ名称編集用##./
    VAR       ?WKSTN    ,NAME
    VAR       ?WS       ,STRING*8,VALUE-'        '
    VAR       ?MEMO     ,STRING*5,VALUE-'     '
    VAR       ?SOKCD    ,STRING*2,VALUE-'  '
    VAR       ?S        ,STRING*1,VALUE-'S'
    VAR       ?W        ,STRING*1,VALUE-'W'
    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'TOKFLIB '
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?RYOJNL1  ,NAME!MOD
    VAR       ?RYOWK    ,NAME!MOD
    VAR       ?HRYOJNL  ,NAME!MOD
    VAR       ?NHRYOJNL ,STRING*17,VALUE-'                 '
    VAR       ?NRYOJNL1 ,STRING*17,VALUE-'                 '
    VAR       ?WKRYO    ,STRING*17,VALUE-'                 '

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL TOKFLIB/TOKELIB
/.##対象ﾒﾓ番号取得##./
SKY1406B:

    ?STEP :=   'SKY1406B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1406B.TOKELIB,PARA-(?WS,?MEMO)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX := '## 量販店ﾒﾓNO = ' && ?MEMO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫コード取得##./
SKY1405B:

    ?STEP :=   'SKY1405B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1405B.TOKELIB,PARA-(?WS,?SOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX := '## 倉庫ｺｰﾄﾞ   = ' && ?SOKCD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ名称取得##./
    ?FILNM    :=    ?S && ?MEMO && '1'
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?RYOJNL1  :=    %NCAT(?FILID,?LIBID)
    ?NRYOJNL1 :=    %STRING(?RYOJNL1)
    ?MSGX     :=    '## ｽﾎﾟｯﾄ出荷F(LF1) = ' && ?NRYOJNL1
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##スポット出荷入力##./
SKY1403I:

    ?STEP :=   'SKY1403I '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-JHMTANL1,TOFILE-JHMTANL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-JHTSPTL1,TOFILE-?RYOJNL1
    CALL      PGM-SKY1403I.TOKELIB,PARA-(?SOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'


    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
