/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    PNJ80100                             *  ./
/. *   JOB-NAME    :    在庫引当／売上更新                   * ./
/. *               :    2018/05/08 ｼﾞｭﾁｭｳｻﾞﾝｹｲｻﾝﾂｲｶ          *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TORICD)
/.###ﾊﾟﾗﾒﾀｴﾘｱ定義####./
    PARA ?HIDUKE  ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA ?JIKAN   ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA ?TORICD  ,STRING*8,IN,VALUE-'        ' /.受信取引先./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PNJ80100'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?KBN      ,STRING*1,VALUE-' '           /.在庫ｽﾗｳﾄﾞ./
    VAR ?KANRINO  ,STRING*8,VALUE-'        '    /.管理番号./
    VAR ?KEKKA    ,STRING*2,VALUE-'  '          /.結果./
    VAR ?PRTBUMON ,STRING*4,VALUE-'    '        /.出力部門./
    VAR ?NOUHINBI ,STRING*8,VALUE-'00000000'    /.納品日./
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB ##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB/TOKDTLIB/
            TOKSOLIB/TOKMDLIB

/.##出力部門取得##./
SKY9999B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SKY9999B.TOKELIBO,PARA-(?TORICD,?PRTBUMON)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##出力部門取得##./
BUMONPRT:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.## 2016/08/02 出力先制御変更　ナフコのみ九州へ
    IF  ?PRTBUMON = '2940'  THEN
        ?MSGX :=  '## 出力部門 = ' && ?PRTBUMON && ' ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        CHGCMVAR '@OUTQN',XXPAMGQ
    ELSE
        CHGCMVAR '@OUTQN',XSYSLSTQ
    END
./  IF  ?TORICD = '00137607'  THEN
        ?MSGX :=  '## 出力場所（九州） = ' && ?PRTBUMON && ' ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        CHGCMVAR '@OUTQN',XXPAMGQ
    ELSE
        ?MSGX :=  '## 出力部門（本社） = ' && ?PRTBUMON && ' ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        CHGCMVAR '@OUTQN',XSYSLSTQ
    END

/.##在庫引当／売上更新##./
SNJ8010B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    OVRF      FILE-JHSHENL3,TOFILE-JHSHENL3.TOKFLIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-SHTDENWK,TOFILE-SHTDENWK.TOKFLIB
    OVRF      FILE-DENKANL1,TOFILE-DENKANL1.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
                                           /.##論理宛先置換え##./
    OVRVLDF   FILE-VLD600,TOFILE-LD600.XUCL
    CALL      PGM-SNJ8010B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TORICD)
    IF        @PGMEC    ^=   0
         THEN
              OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL
              CALL SNJ9060L.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TORICD,'02')
              CALL SNJ9100B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TORICD,'9','K547')
              GOTO ABEND
         ELSE
              OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL
              CALL SNJ9060L.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TORICD,'01')
              CALL SNJ9100B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TORICD,'1','K523')
    END

/.##ケーヨー以外は変換処理へ##./
    IF        ?TORICD ^= '00000173' THEN
              GOTO FLGKAOJO END

/.##今回発注数量更新処理##./
TKY0105B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    OVRF      FILE-ZAMZAIF,TOFILE-ZAMZAIF.TOKFLIB
/.## 未使用の為、起動を停止
    CALL      PGM-TKY0105B.TOKELIB,PARA-(?KBN)   ##./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##売上更新制御クリア##./
FLGKAOJO:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    OVRF      FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
    CALL      PGM-SNJ9050B.TOKELIBO,PARA-('1')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##出荷状況ファイル削除##./
SKZ0011B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-SYUJISL1,TOFILE-SYUJISL1.TOKKLIB
    CALL      PGM-SKZ0011B.TOKELIBO,PARA-('1',?HIDUKE,?JIKAN,?TORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##出荷状況ファイル作成##./
SKZ0010B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-SHTDENLJ,TOFILE-SHTDENLJ.TOKKLIB
    OVRF      FILE-SYUJISL1,TOFILE-SYUJISL1.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SKZ0010B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##2013/02/24 NAV ADD ST##./
/.##オンライン連携状況ファイル作成前削除##./
SBT0155B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-LNKONLL1,TOFILE-LNKONLL1.TOKKLIB
    CALL      PGM-SBT0155B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##オンライン連携状況ファイル作成##./
SBT0150B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-SHTDENLJ,TOFILE-SHTDENLJ.TOKKLIB
    OVRF      FILE-LNKONLL1,TOFILE-LNKONLL1.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SBT0150B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.##2013/02/24 NAV ADD ED##./

/.##2018/04/02 NAV ADD ST##./
/.##受注残ファイル作成##./
SJZ0100B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJZ0100B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.##2018/04/02 NAV ADD ED##./

/.##ナフコ以外は終了へ##./
    IF        ?TORICD ^= '00137607' THEN
              GOTO RTN END

/.##ナフコ基本情報作成処理 2012/03/28 ADD##./
NJH3752B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-NFJOHOL2,TOFILE-NFJOHOL2.TOKDLIB
    OVRF      FILE-SAKUBAL1,TOFILE-SAKUBAL1.TOKDLIB
    OVRF      FILE-NFKANRL1,TOFILE-NFKANRL1.TOKDLIB
    OVRF      FILE-NFSHOMS1,TOFILE-NFSHOMS1.TOKDLIB
    CALL      PGM-NJH3752B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TORICD,
                                          ?KANRINO)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##ナフコ管理番号リスト##./
SSY3751T:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    CHGCMVAR '@OUTQN',XXPAMGQ
    OVRF FILE-NFKANRL1,TOFILE-NFKANRL1.TOKDLIB
    OVRF FILE-TANMAS1,TOFILE-TANMS1.TOKKLIB
    CALL SSY3751T.TOKELIBO,PARA-('1',?KANRINO,?HIDUKE,?JIKAN,?TORICD,
                                 '2940',?KEKKA)
    CHGCMVAR '@OUTQN',XSYSLSTQ
    IF   @PGMEC ^= 0 THEN
         GOTO      ABEND
    END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    CHGCMVAR '@OUTQN',XSYSLSTQ

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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    DSPLOG CLR-@YES,EDT-@JEF
    CHGCMVAR '@OUTQN',XSYSLSTQ

    RETURN    PGMEC-@PGMEC
