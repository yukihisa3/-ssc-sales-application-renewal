/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ナフコ出荷支援システム               *  ./
/. *   JOB-ID      :    PSY38120                             *  ./
/. *   JOB-NAME    :    出荷作場修正（数量訂正）入力　　　　 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY38120'
    VAR       ?STEP     ,STRING*8
    VAR       ?WS       ,STRING*08,VALUE-'        ' /.WKSTN名./
    VAR       ?WKSTN    ,NAME                       /.WKSTN名前./
    VAR       ?TANCD6   ,STRING*06,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*02,VALUE-'  '       /.担当者./
    VAR       ?BUMON    ,STRING*04,VALUE-'    '     /.部門./
    VAR       ?TANNO    ,STRING*03,VALUE-'   '      /.端末番号./
    VAR       ?LNK      ,STRING*3,VALUE-'LNK'
    VAR       ?LNK1     ,STRING*1,VALUE-'1'
    VAR       ?LNKF     ,STRING*1,VALUE-'F'
    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'LINKSLIB'
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?LNKSAIL1 ,NAME!MOD
    VAR       ?LNKSAIF  ,NAME!MOD
    VAR       ?NLNKSAI1,STRING*17,VALUE-'                 '
    VAR       ?NLNKSAIF ,STRING*17,VALUE-'                 '
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '出荷作場修正（数量訂正）入力'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB/BMSFLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##端末番号を取得##./
SBT0620B:

    ?STEP :=   'SBT0620B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SBT0620B.TOKELIBO,PARA-(?WS,?TANNO)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX := '## 端末NO = ' && ?TANNO
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌｧｲﾙ名称取得##./
    ?FILNM    :=    ?LNK && ?TANNO && ?LNKF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?LNKSAIF  :=    %NCAT(?FILID,?LIBID)
    ?NLNKSAIF :=    %STRING(?LNKSAIF)
    ?MSGX     :=    '## LINKS再計算F(PF)  = ' && ?NLNKSAIF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?LNK && ?TANNO && ?LNK1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?LNKSAIL1 :=    %NCAT(?FILID,?LIBID)
    ?NLNKSAI1:=    %STRING(?LNKSAIL1)
    ?MSGX     :=    '## LINKS再計算F(LF)  = ' && ?NLNKSAI1
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##資源の獲得##./
ASS:
     ASSIGN FILE-?LNKSAIF!@XCL
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

    CLRFILE ?LNKSAIF
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '連携状況再計算Ｆ初期化'
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
              ?KEKA4 := 'ログインユーザー情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD6,1,2)


/.##出荷作場修正（数量訂正）入力##./
SSY3812I:

    ?STEP :=   'SSY3812I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LNKSAIL1,TOFILE-?LNKSAIL1
    CALL      PGM-SSY3812I.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '出荷作場修正（数量訂正）入力'
              GOTO ABEND END

/.##連携オンライン状況ファイル再作成##./
SBT0930B:

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
    CALL      PGM-SBT0930B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '連携オンライン状況ファイル再作成'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '出荷作場修正（数量訂正）入力が異常終了'
    ?KEKA2 :=  'しました。ログを採取しＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
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