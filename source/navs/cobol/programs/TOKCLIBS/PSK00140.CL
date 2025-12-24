/. ***テスト用************************************************  ./
/. *     サカタのタネ　ＨＧ部基幹システム　　　　　          *  ./
/. *   SYSTEM-NAME :    ケーヨー伝票レスシステム  　　　     *  ./
/. *   JOB-ID      :    PSK00140                             *  ./
/. *   JOB-NAME    :    指定バッチ番号削除                   * ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSK00140'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?HIDUKE   ,STRING*8,VALUE-'        '    /.ﾊﾞｯﾁNO.:ﾋﾂﾞｹ./
    VAR ?JIKAN    ,STRING*4,VALUE-'    '        /.ﾊﾞｯﾁNO.:ｼﾞｶﾝ./
    VAR ?TORCD    ,STRING*8,VALUE-'        '    /.ﾊﾞｯﾁNO.:ﾄﾘﾋｷｻｷ./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKJLIB

/.##ﾊﾞｯﾁNO.入力画面##./
SSK0014I:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL      PGM-SSK0014I.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TORCD)
    IF        @PGMEC    ^=   0    THEN
              IF  @PGMEC = 4010   THEN
                  ?MSGX := '##取消終了##'
                  SNDMSG ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                  RETURN
              ELSE
                  GOTO ABEND
              END
    END

    ?MSGX :=  '# パラ日付   = ' && ?HIDUKE && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '# パラ時刻   = ' && ?JIKAN  && '     #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX :=  '# パラ取引先 = ' && ?TORCD  && ' #'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-?PGMEC

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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    DSPLOG CLR-@YES,EDT-@JEF

    RETURN    PGMEC-?PGMEC
