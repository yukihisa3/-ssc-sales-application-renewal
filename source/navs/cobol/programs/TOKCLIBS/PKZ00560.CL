/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    業務改善（出荷改善）                 *  ./
/. *   JOB-ID      :    PKZ00560                             *  ./
/. *   JOB-NAME    :    出荷依頼リスト発行（自動出力）　　   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PKZ00560'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'2920'     /.部門CD./
    VAR       ?TANTO    ,STRING*2,VALUE-'NV'       /.担当者CD./
    VAR       ?SOKCD    ,STRING*2,VALUE-'  '       /.倉庫./
    VAR       ?SOKCD1   ,STRING*2,VALUE-'  '       /.倉庫./
    VAR       ?DSOKCD   ,STRING*2,VALUE-'  '       /.代表倉庫./
    VAR       ?SNOUDT   ,STRING*8,VALUE-'        ' /.納品日開始./
    VAR       ?ENOUDT   ,STRING*8,VALUE-'        ' /.納品日終了./
    VAR       ?SSAKUSEI ,STRING*8,VALUE-'        ' /.作成日開始./
    VAR       ?ESAKUSEI ,STRING*8,VALUE-'        ' /.作成日終了./
    VAR       ?TORICD   ,STRING*8,VALUE-'        ' /.取引先CD./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?DTKBN    ,STRING*1,VALUE-' '        /.ＤＴ区分./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKFLIBO

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END
    ?SOKCD1 :=  ?SOKCD

/.##出荷依頼リスト発行条件作成##./
SKZ0056B:

    ?STEP :=   'SKZ0056B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SKZ0056B.TOKELIBO,PARA-(?BUMON,?SOKCD,?DSOKCD,
              ?SNOUDT,?ENOUDT,?SSAKUSEI,?ESAKUSEI,?TORICD,?TANCD,
              ?DTKBN)
    IF        @PGMEC    ^=   0  THEN
              GOTO  ABEND
    END

/.##出荷依頼リスト##./
SKZ0055L:

    ?STEP :=   'SKZ0055L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRPRTF   FILE-XU04LP,TOFILE-LBPPRTF.XUCL,OUTQ-XOUTQ1
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SKZ0057L.TOKELIBO,PARA-(?BUMON,?SOKCD,?DSOKCD,
              ?SNOUDT,?ENOUDT,?SSAKUSEI,?ESAKUSEI,?TORICD,?TANCD,
              ?DTKBN)
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