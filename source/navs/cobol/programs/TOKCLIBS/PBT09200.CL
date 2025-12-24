/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ジュンテンドーＴＣ納品オンライン     *  ./
/. *   JOB-ID      :    PBT09200                             *  ./
/. *   JOB-NAME    :    ＴＣ発注伝票訂正入力－ＬＩＮＫＳ連携 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PBT09200'
    VAR       ?STEP     ,STRING*8
    VAR       ?WS       ,STRING*08,VALUE-'        ' /.WKSTN名./
    VAR       ?WKSTN    ,NAME                       /.WKSTN名前./
    VAR       ?TANCD6   ,STRING*06,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*02,VALUE-'  '       /.担当者./
    VAR       ?BUMON    ,STRING*04,VALUE-'    '     /.部門./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄの登録##./
    DEFLIBL LIBL-TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/TOKWLIB/
            OSKFLIB/TOKCLIBO/OSKELIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##資源の獲得##./
ASS:
     ASSIGN FILE-LNKSAIF.TOKKLIB!@XCL
     IF     @PGMEC ^=  0  THEN
       ?OPR1 := '　＃＃＃＃＃＃　伝票訂正関係処理　＃＃＃＃＃＃＃　'
       ?OPR2 := '　＃＜　ＬＩＮＫＳ伝票訂正　連携状況Ｆ再計算＞＃　'
       ?OPR3 := '　＃他の端末にて資源が使用されております。　　＃　'
       ?OPR4 := '　＃確認して下さい。（全端末未使用ですか？）　＃　'
       ?OPR5 := '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
       CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
       GOTO   ASS     END

/.##連携状況再計算Ｆ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE LNKSAIF.TOKKLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD6)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD6,1,2)

/.##ＴＣ発注伝票訂正入力##./
SBT0920I:

    ?STEP :=   'SBT0920I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB/TOKFLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-OSKELIB
    OVRF      FILE-JTCHEDL1,TOFILE-JTCHEDL1.OSKFLIB
    OVRF      FILE-JTCMEIL2,TOFILE-JTCMEIL2.OSKFLIB
    OVRF      FILE-JTCMEOL1,TOFILE-JTCMEOL1.OSKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-LNKSAIL1,TOFILE-LNKSAIL1.TOKKLIB
    CALL      PGM-SBT0920I.OSKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##連携オンライン状況ファイル再作成##./
SBT0930B:

    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/TOKCLIBO
    ?STEP :=   'SBT0930B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*************************************'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*   連携オンライン状況再計算中！！  *'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*************************************'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LNKONLL1,TOFILE-LNKONLL1.TOKKLIB
    CALL      PGM-SBT0950B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

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