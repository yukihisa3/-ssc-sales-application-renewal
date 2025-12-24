/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理・内部統制対応               *  ./
/. *   JOB-ID      :    PPD01200                             *  ./
/. *   JOB-NAME    :    PC側　手書取込データ　　量販店入力   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PPD01200'
    VAR       ?STEP     ,STRING*8
/.##ﾌｧｲﾙ名称編集用##./
    VAR       ?WKSTN    ,NAME
    VAR       ?WS       ,STRING*8,VALUE-'        '
    VAR       ?MEMO     ,STRING*5,VALUE-'     '
    VAR       ?R        ,STRING*1,VALUE-'R'
    VAR       ?W        ,STRING*1,VALUE-'W'
    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'TOKWLIB '
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?RYOJNL1  ,NAME!MOD
    VAR       ?RYOWK    ,NAME!MOD
    VAR       ?HRYOJNL  ,NAME!MOD
    VAR       ?NHRYOJNL ,STRING*17,VALUE-'                 '
    VAR       ?NRYOJNL1 ,STRING*17,VALUE-'                 '
    VAR       ?WKRYO    ,STRING*17,VALUE-'                 '
    VAR       ?TANCD    ,STRING*2,VALUE-'  '
    VAR       ?BUMON    ,STRING*4,VALUE-'    '
    VAR       ?TANBUM   ,STRING*6
    VAR       ?PGNM     ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                 /.      2    ./
    VAR       ?KEKA2    ,STRING*40                 /.      3    ./
    VAR       ?KEKA3    ,STRING*40                 /.      4    ./
    VAR       ?KEKA4    ,STRING*40                 /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '（手書連携）量販店入力'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  '        '
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKFLIB/TOKELIB

/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WS,?BUMON)
    IF        @PGMEC    ^=   0
          THEN
              ?KEKA4 :=  '【部門ＣＤ取得】'
              GOTO ABEND
    END

/.##担当者CD指定##./
SKY6101I:

    ?STEP :=   'SKY6101I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SKY6101I.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0  THEN
              IF  @PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  ?KEKA4 :=  '【担当者ＣＤ入力】'
                  GOTO  ABEND
              END
    END

/.##量販店入力##./
SPD0120I:

    ?STEP :=   'SPD0120I '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-PCRYOJL1,TOFILE-PCRYOJL1.TOKKLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SPD0120I.TOKELIBO,PARA-(?TANCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【量販店入力】'
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '（手書連携）量販店入力が異常終了しまし'
    ?KEKA2 :=  'た。ログリストを採取してＮＡＶへ連絡し'
    ?KEKA3 :=  'て下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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