/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷サブシステム 　       　　　     *  ./
/. *   JOB-ID      :    PSV010NW                             *  ./
/. *   JOB-NAME    :    オンラインＤＴＣＳＶデータ作成　　   *  ./
/. *               :    （イオン九州ＨＷ：発注種類区分追加） *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSV010NW'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
/.###PGﾊﾟﾗﾒﾀｴﾘｱ###./
    VAR ?BATNO    ,STRING*20,VALUE-'                    '
    VAR ?SOKOCD   ,STRING*4,VALUE-'    '
    VAR ?HIDUKE   ,STRING*8,VALUE-'        '
    VAR ?JIKAN    ,STRING*4,VALUE-'    '
    VAR ?TOKCD    ,STRING*8,VALUE-'        '
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    DEFLIBL TOKELIB/TOKFLIB

/.###発注集計表（一覧）出力###./
PSV010NW:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB

    CALL      PGM-CSV0050I.TOKELIB,PARA-(?BATNO,?SOKOCD,
                                         ?HIDUKE,?JIKAN,?TOKCD)
    ?PGMEC := @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              IF ?PGMEC  =   4001 THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF
                  GOTO   RTN
              ELSE
                  GOTO   ABEND
              END
    END

    ?MSGX :=  '##日付     = ' && ?HIDUKE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##時間     = ' && ?JIKAN
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##取引先CD = ' && ?TOKCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##倉庫ｺｰﾄﾞ = ' && ?SOKOCD
    SNDMSG    ?MSGX,TO-XCTL

/.###発注集計データ抽出###./
PSY00110:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-HACYUDT2,TOFILE-HACYUDT2.TOKKLIB

    CALL      PGM-CSV010NW.TOKELIBO,PARA-(?BATNO,?SOKOCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ＣＳＶデータ出力１##./
CSV0099B:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CSVDT3,TOFILE-CSVDT3.TOKKLIB
    OVRF      FILE-HACYUDT2,TOFILE-HACYUDT2.TOKKLIB
    CALL      PGM-CSV011NW.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ＣＳＶデータ転送１##./
PFIMPORT:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CSVDT3.TOKKLIB,PARA-TXTDATAW,UNIT-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

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