/. ***********************************************************  ./
/. *   サカタのタネ殿　本社システム　　　  　　　　　        *  ./
/. *   SYSTEM-NAME :   ＨＧ基幹システム　　　　　　　　　    *  ./
/. *   JOB-ID      :   PBT04000                              *  ./
/. *   JOB-NAME    :   ＬＩＮＫＳ出荷梱包データ取込VER1　　  *  ./
/. *                    （カインズ専用）　　　　　　　　　　 *  ./
/. *                    ※栃木倉庫　　　　　　　　　　　　　 *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM (P1-?TOKCD)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?TOKCD   ,STRING*8,IN,VALUE-'        '
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS      ,STRING*08,VALUE-'        '      /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?BUMON   ,STRING*04,VALUE-'    '          /.部門名       ./
    VAR ?TANCD6  ,STRING*06,VALUE-'      '        /.担当者CDWK   ./
    VAR ?TANCD   ,STRING*02,VALUE-'  '            /.担当者CD     ./
    VAR ?SOUKO   ,STRING*02,VALUE-'  '            /.倉庫コード     ./
    VAR ?DSOUKO  ,STRING*02,VALUE-'  '            /.代表倉庫コード ./
    VAR ?WKSTN   ,NAME                            /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGMEC   ,INTEGER                         /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ ./
    VAR ?PGMECX  ,STRING*11                       /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ    ./
    VAR ?PGMEM   ,STRING*99                       /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ  ./
    VAR ?MSG     ,STRING*99(6)                    /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX    ,STRING*99                         /.SNDMSG表示用 ./
    VAR ?CLID    ,STRING*08,VALUE-'PBT04000'        /.CLID         ./
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
    VAR ?CHK     ,STRING*1,VALUE-' '                /.存在ﾁｪｯｸ   ./
    VAR ?KBN     ,STRING*1,VALUE-'1'                /.ｴﾗｰのみ出力./

   /.##抽出件数情報  ##./
    VAR ?KENSU1  ,STRING*8,VALUE-'00000000'
    VAR ?KENSU2  ,STRING*8,VALUE-'00000000'
    VAR ?KENSU3  ,STRING*8,VALUE-'00000000'
    VAR ?KENSU4  ,STRING*8,VALUE-'00000000'
    VAR ?KENSU5  ,STRING*8,VALUE-'00000000'
    VAR ?KENSU6  ,STRING*8,VALUE-'00000000'
    VAR ?KENSU7  ,STRING*8,VALUE-'00000000'
   /.##ファイル名編集##./
    VAR ?CNZLNK  ,STRING*6,VALUE-'CNZLNK'
    VAR ?CNZLWK  ,STRING*6,VALUE-'CNZLWK'
    VAR ?SYKME   ,STRING*5,VALUE-'SYKME'
    VAR ?SYKKP   ,STRING*5,VALUE-'SYKKP'
    VAR ?SYKCK   ,STRING*5,VALUE-'SYKCK'
    VAR ?FILNM   ,STRING*8,VALUE-'        '
    VAR ?LIBNM   ,STRING*8,VALUE-'LINKSLIB'
    VAR ?KEY1    ,STRING*1,VALUE-'1'
    VAR ?KEY2    ,STRING*1,VALUE-'2'
    VAR ?KEY3    ,STRING*1,VALUE-'3'
    VAR ?FILID   ,NAME
    VAR ?LIBID   ,NAME
    VAR ?CNZLNKF ,NAME!MOD
    VAR ?CNZLNKWK,NAME!MOD
    VAR ?SYKMEIF ,NAME!MOD
    VAR ?SYKKEPF ,NAME!MOD
    VAR ?SYKCHKF ,NAME!MOD
    VAR ?SYKCHKL1,NAME!MOD
    VAR ?SYKCHKL2,NAME!MOD
    VAR ?SYKCHKL3,NAME!MOD
    VAR ?NNZLNKF ,STRING*17,VALUE-'                 '
    VAR ?NNZLNKWK,STRING*17,VALUE-'                 '
    VAR ?NYKMEIF ,STRING*17,VALUE-'                 '
    VAR ?NYKKEPF ,STRING*17,VALUE-'                 '
    VAR ?NYKCHKF ,STRING*17,VALUE-'                 '
    VAR ?NYKCHKL1,STRING*17,VALUE-'                 '
    VAR ?NYKCHKL2,STRING*17,VALUE-'                 '
    VAR ?NYKCHKL3,STRING*17,VALUE-'                 '

/.==================================================================./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := 'ＬＩＮＫＳ出荷梱包データ取込　　　　　'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/BMSFLIB/LINKSLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.==================================================================./

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
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD6,1,2)

/.==================================================================./

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
    ?SOUKO := 'TK'
    ?DSOUKO := '01'

/.==================================================================./

/.##ファイル名作成##./
PFILECHG:

    ?STEP :=   'PFILECHG'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌｧｲﾙ名称取得（倉庫別出荷梱包ファイル取込時　　）##./
    ?FILNM    :=    ?CNZLWK && ?SOUKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CNZLNKWK :=    %NCAT(?FILID,?LIBID)
    ?NNZLNKWK :=    %STRING(?CNZLNKWK)
    ?MSGX     :=    '## 出荷梱包F取込時  = ' && ?NNZLNKWK
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌｧｲﾙ名称取得（倉庫別出荷梱包ファイル　　　　　）##./
    ?FILNM    :=    ?CNZLNK && ?SOUKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CNZLNKF  :=    %NCAT(?FILID,?LIBID)
    ?NNZLNKF  :=    %STRING(?CNZLNKF)
    ?MSGX     :=    '## 出荷梱包F        = ' && ?NNZLNKF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌｧｲﾙ名称取得（倉庫別出荷取引情報ファイル　　　）##./
    ?FILNM    :=    ?SYKME && ?SOUKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SYKMEIF  :=    %NCAT(?FILID,?LIBID)
    ?NYKMEIF  :=    %STRING(?SYKMEIF)
    ?MSGX     :=    '## 出荷取引情報Ｆ　 = ' && ?NYKMEIF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌｧｲﾙ名称取得（倉庫別出荷欠品情報ファイル　　　）##./
    ?FILNM    :=    ?SYKKP && ?SOUKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SYKKEPF  :=    %NCAT(?FILID,?LIBID)
    ?NYKKEPF  :=    %STRING(?SYKKEPF)
    ?MSGX     :=    '## 出荷欠品情報Ｆ   = ' && ?NYKKEPF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌｧｲﾙ名称取得（出荷梱包チェックワークＰＦ　　　）##./
    ?FILNM    :=    ?SYKCK && ?SOUKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SYKCHKF  :=    %NCAT(?FILID,?LIBID)
    ?NYKCHKF  :=    %STRING(?SYKCHKF)
    ?MSGX     :=    '## 出荷梱包ﾁｪｯｸ(PF) = ' && ?NYKCHKF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌｧｲﾙ名称取得（出荷梱包チェックワークＬＦ１　　）##./
    ?FILNM    :=    ?SYKCK && ?SOUKO && ?KEY1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SYKCHKL1 :=    %NCAT(?FILID,?LIBID)
    ?NYKCHKL1 :=    %STRING(?SYKCHKL1)
    ?MSGX     :=    '## 出荷梱包ﾁｪｯｸ(LF1)= ' && ?NYKCHKL1
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌｧｲﾙ名称取得（出荷梱包チェックワークＬＦ２　　）##./
    ?FILNM    :=    ?SYKCK && ?SOUKO && ?KEY2
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SYKCHKL2 :=    %NCAT(?FILID,?LIBID)
    ?NYKCHKL2 :=    %STRING(?SYKCHKL2)
    ?MSGX     :=    '## 出荷梱包ﾁｪｯｸ(LF2)= ' && ?NYKCHKL2
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##ﾌｧｲﾙ名称取得（出荷梱包チェックワークＬＦ３　　）##./
    ?FILNM    :=    ?SYKCK && ?SOUKO && ?KEY3
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?SYKCHKL3 :=    %NCAT(?FILID,?LIBID)
    ?NYKCHKL3 :=    %STRING(?SYKCHKL3)
    ?MSGX     :=    '## 出荷梱包ﾁｪｯｸ(LF3)= ' && ?NYKCHKL3
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.==================================================================./

/.##ファイル獲得##./
PASSING:

    ?STEP :=   'PASSING '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN FILE-?CNZLNKF!@XCL/?SYKMEIF!@XCL/?SYKKEPF!@XCL
               /?SYKCHKF!@XCL
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA1 :=  '＃＃＃ＬＩＮＫＳ出荷梱包ＤＴ取込＃＃＃'
              ?KEKA2 :=  '他端末又は他業務でファイルの獲得が行な'
              ?KEKA3 :=  'われております。他端末で未使用を確認し'
              ?KEKA4 :=  '再度処理をお願い致します。'
              GOTO ABEND1
    END

/.==================================================================./

/.##確認画面##./
SHOM0900:

    ?STEP :=   'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '＃＃＃ＬＩＮＫＳ出荷梱包ＤＴ取込＃＃＃'
    ?OPR2  :=  'ＬＩＮＫＳ側の出荷梱包データ取込を行な'
    ?OPR3  :=  'います。ＬＩＮＫＳ側データセットを確認'
    ?OPR4  :=  'して下さい。'
    ?OPR5  :=  '※前回分未送信の場合は、取込出来ません'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '確認画面'
              GOTO ABEND
    END

/.==================================================================./

/.##倉庫別出荷梱包データ存在チェック##./
SBT0400B:

    ?STEP :=   'SBT0400B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CNZLNKF,TOFILE-?CNZLNKF
    CALL      PGM-SBT0400B.TOKELIBO,PARA-(?CHK)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '倉庫別出荷梱包データ存在チェック'
              GOTO ABEND
    END
    ?MSGX :=  '## KEKKA = ' && ?CHK   &&   ' ##*'
    SNDMSG    ?MSGX,TO-XCTL
  /.##チェック区分＝０（未送信の場合）異常終了させる##./
    IF        ?CHK      =  '0'  THEN
              ?KEKA1 :=  '＃＃＃ＬＩＮＫＳ出荷梱包ＤＴ取込＃＃＃'
              ?KEKA2 :=  '出荷梱包データが未送信状態です。'
              ?KEKA3 :=  '送信済か確認し、再度、取込処理を実行し'
              ?KEKA4 :=  'て下さい。'
              GOTO ABEND1
    END

/.==================================================================./

/.##倉庫別出荷梱包データ取込##./
PFEXPORT:

    ?STEP :=   'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-?CNZLNKWK,MODE-@REP,PARA-LNKKNPDT,
            UNIT-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '倉庫別出荷梱包データ取込'
              GOTO ABEND
    END

/.==================================================================./

/.##取込データレコード長編集##./
SBT0630B:

    ?STEP :=   'SBT0630B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CNZLNKWK,TOFILE-?CNZLNKWK
    OVRF      FILE-CNZLNKF,TOFILE-?CNZLNKF
    CALL      PGM-SBT0630B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '取込データレコード長編集'
              GOTO ABEND
    END

/.==================================================================./

/.##カインズ出荷梱包ＭＳＧ取込##./
SBT0410B:

    ?STEP :=   'SBT0410B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CNZLNK,TOFILE-?CNZLNKF
    OVRF      FILE-SYKMEF,TOFILE-?SYKMEIF
    OVRF      FILE-SYKKPF,TOFILE-?SYKKEPF
    CALL      PGM-SBT0410B.TOKELIBO,
              PARA-(?KENSU1,?KENSU2,?KENSU3,?KENSU4,?KENSU5,
                    ?KENSU6,?KENSU7)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'カインズ出荷梱包ＭＳＧ取込'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷梱包データ件数リスト##./
SBT0420L:

    ?STEP :=   'SBT0420L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CASE  ?SOUKO  OF
       # 'TU' #
       ?MSGX :=  '##　ＡＷ札幌配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTU.TOKELIBO,MEDLIB-TOKELIBO
       # '6A' #
       ?MSGX :=  '##　片岡配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT6A.TOKELIBO,MEDLIB-TOKELIBO
       # 'TK' #
       ?MSGX :=  '##　さくら配送センター　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTK.TOKELIBO,MEDLIB-TOKELIBO
       # '63' #
       ?MSGX :=  '##　フバサミ配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT63.TOKELIBO,MEDLIB-TOKELIBO
       # '84' #
       ?MSGX :=  '##　鴻巣配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT84.TOKELIBO,MEDLIB-TOKELIBO
       # 'TV' #
       ?MSGX :=  '##　西尾配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTV.TOKELIBO,MEDLIB-TOKELIBO
       # 'T5' #
       ?MSGX :=  '##　ＫＡ岡山配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTT5.TOKELIBO,MEDLIB-TOKELIBO
       # 'E1' #
       ?MSGX :=  '##　甘木配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTE1.TOKELIBO,MEDLIB-TOKELIBO
       # '01' #
       ?MSGX :=  '##　本社　　　　　　　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO,MEDLIB-TOKELIBO
    ELSE
       ?MSGX :=  '##　出力判定不能　本社へ出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO,MEDLIB-TOKELIBO
    END
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SBT0420L.TOKELIBO
             ,PARA-(?TOKCD,?SOUKO,?TANCD,?BUMON,
                    ?KENSU1,?KENSU2,?KENSU3,?KENSU4,?KENSU5,
                    ?KENSU6,?KENSU7)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷梱包データ件数リスト'
              GOTO ABEND
    END

/.==================================================================./

/.##ＳＯＲＴ処理（取引情報ワーク）##./
PSORT1:

    ?STEP :=   'PSORT1  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT       INFILE-?SYKMEIF,INRL-1200,INBF-1,
              OUTFILE-?SYKMEIF,OUTBF-1,
              KEY-24!8!CA,KEY1-2!10!CA,KEY2-12!4!CA,
              RCDL-@DSP
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＳＯＲＴ処理（取引情報ワーク）'
              GOTO ABEND
    END

/.==================================================================./

/.##ＳＯＲＴ処理（欠品情報ワーク）##./
PSORT2:

    ?STEP :=   'PSORT2  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT       INFILE-?SYKKEPF,INRL-1200,INBF-1,
              OUTFILE-?SYKKEPF,OUTBF-1,
              KEY-39!8!CA,KEY1-17!10!CA,KEY2-27!4!CA,
              RCDL-@DSP
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＳＯＲＴ処理（欠品情報ワーク）'
              GOTO ABEND
    END

/.==================================================================./

/.##出荷梱包チェックワーク初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   FILE-?SYKCHKF
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷梱包チェックワーク初期化'
              GOTO ABEND
    END

/.==================================================================./

/.##カインズ出荷梱包ＭＳＧ→チェック用ファイル編集##./
SBT0430B:

    ?STEP :=   'SBT0430B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SYKMEF,TOFILE-?SYKMEIF
    OVRF      FILE-SYKKPF,TOFILE-?SYKKEPF
    OVRF      FILE-SYKCKL3,TOFILE-?SYKCHKL3
    CALL  PGM-SBT0430B.TOKELIBO,PARA-(?TOKCD,?SOUKO,?TANCD,?BUMON)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'カインズ出荷梱包ＭＳＧチェック編集'
              GOTO ABEND
    END

/.==================================================================./

/.##カインズ発注（出荷指示）との照合##./
SBT0440B:

    ?STEP :=   'SBT0440B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SYKCKL1,TOFILE-?SYKCHKL1
    OVRF      FILE-CNZSYRL4,TOFILE-CNZSYRL4.BMSFLIB
    CALL      PGM-SBT0440B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'カインズ発注（出荷指示）との照合'
              GOTO ABEND
    END

/.==================================================================./

/.##カインズ事前出荷データチェックリスト##./
SBT0450L:

    ?STEP :=   'SBT0450L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SYKCKL2,TOFILE-?SYKCHKL2
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SBT0450L.TOKELIBO,PARA-(?KBN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'カインズ事前出荷データチェックリスト'
              GOTO ABEND
    END

/.==================================================================./

/.##ＵＳＢ内出荷梱包ファイル０クリア##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-CLRKONPO.TOKWLIB,PARA-LNKKNPDT,UNIT-2
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＵＳＢ内出荷梱包ファイル０クリア'
              GOTO ABEND
    END

/.==================================================================./

/.##正常終了処理##./
RTN:

    RELEASE   ?CNZLNKF!@XCL
    RELEASE   ?SYKMEIF!@XCL
    RELEASE   ?SYKKEPF!@XCL
    RELEASE   ?SYKCHKF!@XCL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  'ＬＩＮＫＳ出荷梱包データ取込処理が'
    ?KEKA2 :=  '正常終了しました。'
    ?KEKA3 :=  'エラーがある場合は、エラー内容を確'
    ?KEKA4 :=  '認し修正をお願い致します。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC
/.==================================================================./

/.##異常終了処理##./
ABEND:

    RELEASE   ?CNZLNKF!@XCL
    RELEASE   ?SYKMEIF!@XCL
    RELEASE   ?SYKKEPF!@XCL
    RELEASE   ?SYKCHKF!@XCL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
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

/.##未送信時終了処理##./
ABEND1:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
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