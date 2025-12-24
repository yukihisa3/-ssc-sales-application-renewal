/. ***********************************************************  ./
/. *   サカタのタネ　営業第２部　　　　　　　　　　　 　     *  ./
/. *   SYSTEM-NAME :    WEBEDI実行制御　　　　　　　　　　   *  ./
/. *   JOB-ID      :    PDCM0000                             *  ./
/. *   JOB-NAME    :    （自動用）実行処理判定制御　　　　　 *  ./
/. *                              　　　　　　　　　　　　　 *  ./
/. *   CREATE      :    2022/06/29 NAV TAKAHASHI             *  ./
/. *   流用元      :    PDCM0010.TOKCLLIB                    *  ./
/. *<修正履歴>************************************************  ./
/. *   XXXX/XX/XX XXXXXXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮ *  ./
/. *   XXXX/XX/XX XXXXXXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮ *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECN ,STRING*8
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PDCM0000'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,NAME
    VAR       ?WS     ,STRING*8,VALUE-'        '
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-' '         /.実行チェック./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./


    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/TOKDTLIB/TOKSOLIB/
            TOKCOLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '（自動用）実行処理判定制御'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ＬＯＧＩＮユーザーコピー#./
STEP010:

    ?STEP :=   'STEP010 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  CNVFILE FILE-LOGINBAK.TOKDTLIB,TOFILE-LOGINUSR.@TEMP,
            ADD-@NO,BF-255
./  CPYFILE FILE-LOGINBAK.TOKDTLIB,TOFILE-LOGINUSR.@TEMP,
            CRTFILE-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF      ?PGMEC    ^=   0    THEN
            ?KEKA4 := '【ＬＯＧＩＮユーザーコピ】'
                    GOTO ABEND
    END

/.##実行制御ファイル取込##./
STEP020:

    ?STEP :=   'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-WEBEDIF.TOKDTLIB,MODE-@REP,PARA-WEBEDIF,
            UNIT-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【実行制御ファイル取込】'
              GOTO ABEND
    END

/.##実行制御ファイル判定##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL DCM0040B.TOKSOLIB,PARA-(?CHK)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【実行制御ファイル判定】'
              GOTO ABEND
    END

    CASE  ?CHK OF
       #  '1'  #  /.ＤＣＭの場合./
           ?MSGX :=  '＃ＤＣＭ発注取込開始＃'
           SNDMSG    ?MSGX,TO-XCTL
           CALL PDCM0010.TOKCOLIB
       #  '2'  #  /.ＬＩＣの場合./
           ?MSGX :=  '＃ＬＩＣ発注取込開始＃'
           SNDMSG    ?MSGX,TO-XCTL
       ELSE
           ?MSGX :=  '＃実行制御無し！！＃'
           SNDMSG    ?MSGX,TO-XCTL
    END

/.##プログラム正常終了##./
RTN:

    SNDMSG    '（自動用）実行処理判定制御　正常終了',
              ,TO-XCTL
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '（自動用）実行処理判定制御が異常終了'
    ?KEKA2 :=  'しました。ログ採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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

    RETURN    PGMEC-?PGMEC
