/. ***TEST****************************************************  ./
/. *     サカタのタネ　本社システム　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   *  ./
/. *   JOB-ID      :    PBT00900                             *  ./
/. *   JOB-NAME    :    オンライン連携状況照会　　　　　　　 *  ./
/. *                                                      　 *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS      ,STRING*08,VALUE-'        '      /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?SOUKO   ,STRING*02,VALUE-'  '            /.倉庫コード     ./
    VAR ?DSOUKO  ,STRING*02,VALUE-'  '            /.代表倉庫コード ./
    VAR ?CHKBN   ,STRING*01,VALUE-' '             /.帳票区分       ./
    VAR ?SSOUKO  ,STRING*02,VALUE-'  '            /.照会倉庫       ./
    VAR ?SHOKBN  ,STRING*01,VALUE-' '             /.照会区分       ./
    VAR ?KJNJBI  ,STRING*08,VALUE-'        '      /.基準受信日     ./
    VAR ?TORCD   ,STRING*08,VALUE-'        '      /.取引先ＣＤ     ./
    VAR ?WKSTN   ,NAME                            /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGMEC   ,INTEGER                         /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ ./
    VAR ?PGMECX  ,STRING*11                       /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ    ./
    VAR ?PGMEM   ,STRING*99                       /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ  ./
    VAR ?MSG     ,STRING*99(6)                    /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX    ,STRING*99                         /.SNDMSG表示用 ./
    VAR ?CLID    ,STRING*08,VALUE-'PBT00900'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./
    VAR ?OPR1    ,STRING*50                         /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2    ,STRING*50                         /.      2    ./
    VAR ?OPR3    ,STRING*50                         /.      3    ./
    VAR ?OPR4    ,STRING*50                         /.      4    ./
    VAR ?OPR5    ,STRING*50                         /.      5    ./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := 'オンライン連携状況照会　　　　'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫コード取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,
                  PARA-(?WS,?SOUKO,?DSOUKO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '倉庫コード取得'
              GOTO ABEND
    END

/.##オンライン連携状況照会##./
SBT0090A:

    ?STEP :=   'SBT0090A'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-LNKONLL2,TOFILE-LNKONLL2.TOKKLIB
    CALL      PGM-SBT0090A.TOKELIBO,PARA-(?SOUKO,?DSOUKO,
                                          ?CHKBN,?SSOUKO,
                                          ?SHOKBN,?KJNJBI,?TORCD)
    IF        @PGMEC    ^=   0  THEN
              IF  @PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  GOTO RTN
              ELSE
                  ?KEKA4 :=  'オンライン連携状況照会'
                  GOTO ABEND
              END
    END
    ?MSGX := '# 帳票区分    = ' && ?CHKBN && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 照会倉庫　    = ' && ?SSOUKO && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 照会区分　　  = ' && ?SHOKBN && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 基準受信日　 = ' && ?KJNJBI && ' #'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX := '# 取引先　　　 = ' && ?TORCD && ' #'
    SNDMSG    ?MSGX,TO-XCTL
/.##帳票区分が2:帳票発行の場合のみ、リスト発行##./
    IF        ?CHKBN    ^=   '2'    THEN
              GOTO RTN
    END

/.##オンライン連携状況リスト発行##./
SBT0110L:

    ?STEP :=   'SBT0110L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-PRTFILE,TOFILE-PRTF.TOKELIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-LNKONLL2,TOFILE-LNKONLL2.TOKKLIB
    CALL      PGM-SBT0110L.TOKELIBO,PARA-(?CHKBN,?SSOUKO,
                                          ?SHOKBN,?KJNJBI,?TORCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'オンライン連携状況リスト発行'
              GOTO ABEND
    END


/.##正常終了処理##./
RTN:

    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC
/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?CLID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
