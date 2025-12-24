/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    ナフコＥＤＩ　                       *  ./
/. *   JOB-ID      :    PSY3930S                             *  ./
/. *   JOB-NAME    :    ナフコ　発注一覧ＣＳＶ出力（資材用） *  ./
/. *   UPDATE      :                                         *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY3930S'
    VAR       ?STEP     ,STRING*8

    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./

    VAR       ?SOKCD    ,STRING*2,VALUE-'00'       /.倉庫ＣＤ  ./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./

    VAR       ?HDATEF   ,STRING*8,VALUE-'00000000' /.発注FROM    ./
    VAR       ?HDATET   ,STRING*8,VALUE-'00000000' /.発注TO      ./
    VAR       ?JANCD    ,STRING*13,VALUE-'0000000000000' /.JANCD ./
    VAR       ?TENPOF   ,STRING*3,VALUE-'000'      /.店舗FROM    ./
    VAR       ?TENPOT   ,STRING*3,VALUE-'999'      /.店舗TO      ./
                                                   /.作場 * 20   ./
    VAR       ?SAKUCD   ,STRING*40,
                VALUE-'                                        '

    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'

    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./

/.-------------------------------------------------------------./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
/.-------------------------------------------------------------./
    VAR       ?SYORIKN1,STRING*7,VALUE-'0000000'  /.件数情報   ./

    /.端末別ファイルＩＤ編集用./
           /.端末番号./
    VAR       ?TANNO    ,STRING*03,VALUE-'   '

           /.ナフコ発注一覧ワーク PF  ./
    VAR       ?NFWKXXXF ,NAME!MOD
           /.ナフコ発注一覧ワーク LF1 ./
    VAR       ?NFWK     ,STRING*4,VALUE-'NFWK'
    VAR       ?NFWK1    ,STRING*1,VALUE-'1'
    VAR       ?NFWKXXX1 ,NAME!MOD
           /.ナフコ発注一覧ワーク LF4 ./
    VAR       ?NFWK4    ,STRING*1,VALUE-'4'
    VAR       ?NFWKXXX4 ,NAME!MOD

           /.ナフコ発注一覧ＣＳＶ./
    VAR       ?CSVF     ,STRING*5,VALUE-'NFHAC'
    VAR       ?NFHACXXX ,NAME!MOD

           /.ファイルＩＤ表示用./
    VAR       ?FILENAME ,STRING*17,VALUE-'                 '

    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'TOKDTLIB'
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME

/.----------------------------------------------------------------./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ナフコ　発注一覧ＣＳＶ出力'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKDLIB/TOKSOLIB/
              TOKMDLIB/TOKDTLIB

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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?P7)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 := '倉庫コード取得'
              GOTO ABEND
    END

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.----------------------------------------------------------------./
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

/.##端末別ファイルＩＤ取得##./
   /.ナフコ発注一覧ワーク PF  ./
    ?FILNM    :=    ?NFWK && ?TANNO && 'F'
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?NFWKXXXF :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?NFWKXXXF)
    ?MSGX     :=    '##発注一覧ワーク=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.ナフコ発注一覧ワーク LF1 ./
    ?FILNM    :=    ?NFWK && ?TANNO && ?NFWK1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?NFWKXXX1 :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?NFWKXXX1)
    ?MSGX     :=    '##発注一覧ワーク=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.ナフコ発注一覧ワーク LF4 ./
    ?FILNM    :=    ?NFWK && ?TANNO && ?NFWK4
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?NFWKXXX4 :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?NFWKXXX4)
    ?MSGX     :=    '##発注一覧ワーク=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.ナフコ発注一覧ＣＳＶ./
    ?FILNM    :=    ?CSVF  && ?TANNO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?NFHACXXX :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?NFHACXXX)
    ?MSGX     :=    '##発注一覧ＣＳＶ=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.----------------------------------------------------------------./
/.##ＣＳＶデータ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE   ?NFHACXXX
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4    :=   'ＣＳＶデータ初期化'
              GOTO ABEND
    END

    CLRFILE   ?NFWKXXXF
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4    :=   'ワークファイル初期化'
              GOTO ABEND
    END

/.##ナフコ発注データ累積　※端末別ワークへ累積./
NJH3753B:

    ?STEP :=   'NJH3753B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-NFHACSF,TOFILE-NSHACSF.ONLBLIB
    OVRF      FILE-NFHACL1,TOFILE-?NFWKXXX1
/.##CALL      PGM-NJH3753B.TOKSOLIB,PARA-(?SYORIKN2)
##./
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##データ変換（制御バイト削除）桁ズレ防止の為##./
NJH3753C: /.←S2245053 追加./

    ?STEP :=   'NJH3753B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-NFHACSF,TOFILE-NSHACSF.ONLBLIB
    OVRF      FILE-NFHACWK,TOFILE-NSHACWK.ONLBLIB
    CALL      PGM-NJH3753C.TOKSOLIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##データ変換（制御バイト排除ＤＴから累積）##./
NJH3753D:

    ?STEP :=   'NJH3753B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-NFHACWK,TOFILE-NSHACWK.ONLBLIB
    OVRF      FILE-NFHACL1,TOFILE-NSHACL1.TOKDTLIB
    CALL      PGM-NJH3753D.TOKSOLIB,PARA-(?SYORIKN1)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              GOTO ABEND
    END

/.##ナフコ発注一覧ＣＳＶ出力指示./
SSY3930I:

    ?STEP :=   'SSY3930I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB

    CALL      PGM-SSY3930I.TOKSOLIB,PARA-(?HDATEF,?HDATET,?JANCD,
                                          ?TENPOF,?TENPOT)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ナフコ発注一覧ＣＳＶ出力指示'
              GOTO   ABEND
    END

    ?MSGX :=  '発注日（開始）＝'  && ?HDATEF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '発注日（終了）＝'  && ?HDATET
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  'ＪＡＮコード　＝'  && ?JANCD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '店舗　（開始）＝'  && ?TENPOF
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '店舗　（終了）＝'  && ?TENPOT
    SNDMSG    ?MSGX,TO-XCTL

/.##ナフコ累積データ抽出##./
NJH3755B:

    ?STEP :=   'NJH3755B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF FILE-NFHACL3,TOFILE-NSHACL3.TOKDTLIB
    OVRF FILE-NFWKXXX4,TOFILE-?NFWKXXX4
    CALL      PGM-NJH3755B.TOKSOLIB,PARA-(?HDATEF,?HDATET,?JANCD,
                                          ?TENPOF,?TENPOT)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ナフコ累積データ抽出'
              GOTO ABEND END

/.##ナフコ発注一覧ＣＳＶ出力##./
SSY3942V:

    ?STEP :=  'SSY3942V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    IF        ?JANCD  =  '0000000000000'  THEN
              ?JANCD :=  '             '
    END

    OVRF      FILE-NFWKXXX4,TOFILE-?NFWKXXX4
    OVRF      FILE-NFHACXXX,TOFILE-?NFHACXXX
    CALL      PGM-SSY3942V.TOKSOLIB,PARA-(?HDATEF,?HDATET,?JANCD,
                                          ?TENPOF,?TENPOT)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =  4010  THEN
              GOTO RTN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ナフコ発注一覧ＣＳＶ出力'
              GOTO ABEND
    END

FIMPORT1:   /.ナフコ発注一覧ＣＳＶ転送./

    ?STEP :=  'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-?NFHACXXX,
              TYPE-@FILE,
              PARA-NAFUKO,
            /.UNIT-6,./
              UNIT-8,
              OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ナフコ発注一覧ＣＳＶ転送'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ発注一覧ＣＳＶ作成が異常しました'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
    CALL      SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?PGMEM    :=    ?PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO    ?MSGX  :=   ?MSG(?I)
              SNDMSG      ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
