/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    ナフコ出荷支援                       *  ./
/. *   JOB-ID      :    PSY51700                             *  ./
/. *   JOB-NAME    :    発注確認用データ作成　　　　　　　   *  ./
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
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY51700'
    VAR       ?STEP     ,STRING*8
    /.端末別ファイルＩＤ編集用./
              /.端末番号./
    VAR       ?TANNO    ,STRING*03,VALUE-'   '

              /.発注確認店舗ワーク./
    VAR       ?MEIT     ,STRING*4,VALUE-'MEIT'
    VAR       ?MEITF    ,STRING*1,VALUE-'F'
    VAR       ?MEIT1    ,STRING*1,VALUE-'1'
    VAR       ?MEITXXXF ,NAME!MOD
    VAR       ?MEITXXX1 ,NAME!MOD

              /.発注確認商品明細ワーク./
    VAR       ?MEIS     ,STRING*4,VALUE-'MEIS'
    VAR       ?MEISF    ,STRING*1,VALUE-'F'
    VAR       ?MEIS1    ,STRING*1,VALUE-'1'
    VAR       ?MEISXXXF ,NAME!MOD
    VAR       ?MEISXXX1 ,NAME!MOD

              /.ファイルＩＤ表示用./
    VAR       ?FILENAME ,STRING*17,VALUE-'                 '

              /.発注確認ＣＳＶ（店舗）./
    VAR       ?NCST     ,STRING*4,VALUE-'NCST'
    VAR       ?NCSTF    ,STRING*1,VALUE-'F'
    VAR       ?NCSTXXXF ,NAME!MOD

              /.発注確認ＣＳＶ（商品）./
    VAR       ?NCSM     ,STRING*4,VALUE-'NCSM'
    VAR       ?NCSMF    ,STRING*1,VALUE-'F'
    VAR       ?NCSMXXXF ,NAME!MOD

              /.ＣＳＶファイルＩＤ表示用./
    VAR       ?NHACCSVW ,STRING*17,VALUE-'                 '

    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*8,VALUE-'TOKDTLIB'
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?KANRINO  ,STRING*8,VALUE-'00000000' /.管理番号    ./
    VAR       ?P4       ,STRING*2,VALUE-'0'        /.倉庫ＣＤ  ./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./

    VAR       ?SAKUCD   ,STRING*40,                /.作場ＣＤ　./
                VALUE-'                                        '

    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./
/.-----------------------------------------------------------./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKDTLIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '発注確認用データ作成'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P7)
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
   /.発注確認店舗ワーク./
    ?FILNM    :=    ?MEIT && ?TANNO && ?MEITF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?MEITXXXF :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?MEITXXXF)
    ?MSGX     :=    '##発注確認店舗ワーク=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?MEIT && ?TANNO && ?MEIT1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?MEITXXX1 :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?MEITXXX1)
    ?MSGX     :=    '##                   ' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.発注確認商品明細ワーク./
    ?FILNM    :=    ?MEIS && ?TANNO && ?MEISF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?MEISXXXF :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?MEISXXXF)
    ?MSGX     :=    '##発注確認明細ワーク=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?MEIS && ?TANNO && ?MEIS1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?MEISXXX1 :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?MEISXXX1)
    ?MSGX     :=    '##                   ' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.ＣＳＶ出力用ワーク（店舗）./
    ?FILNM    :=    ?NCST && ?TANNO && ?NCSTF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?NCSTXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACCSVW :=    %STRING(?NCSTXXXF)
    ?MSGX     :=    '##発注ＣＳＶ（店舗）=' && ?NHACCSVW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.ＣＳＶ出力用ワーク（明細）./
    ?FILNM    :=    ?NCSM && ?TANNO && ?NCSMF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?NCSMXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACCSVW :=    %STRING(?NCSMXXXF)
    ?MSGX     :=    '##発注ＣＳＶ（明細）=' && ?NHACCSVW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##端末別ファイル排他##./
FILECHK:
   /.発注確認店舗ワーク./
    ASSIGN FILE-?MEITXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注確認店舗ワーク　排他処理'
           SNDMSG 'ファイル排他獲得１',TOWS-@ORGWS
           GOTO   ERR
    END
   /.発注確認商品明細ワーク./
    ASSIGN FILE-?MEISXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注確認明細ワーク　排他処理'
           SNDMSG 'ファイル排他獲得２',TOWS-@ORGWS
           GOTO   ERR
    END
   /.発注ＣＳＶファイル(店舗)./
    ASSIGN FILE-?NCSTXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注ＣＳＶファイル(店舗)排他処理'
           SNDMSG 'ファイル排他獲得３',TOWS-@ORGWS
           GOTO   ERR
    END

   /.発注ＣＳＶファイル(明細)./
    ASSIGN FILE-?NCSMXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注ＣＳＶファイル(明細)排他処理'
           SNDMSG 'ファイル排他獲得４',TOWS-@ORGWS
           GOTO   ERR
    END

/.##発注確認　データ作成指示##./
SSY5170I:

    ?STEP :=   'SSY5170I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    CALL      PGM-SSY5170I.TOKSOLIB,PARA-(?KANRINO,?SAKUCD)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END

    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注確認データ作成指示'
              GOTO   ABEND
    END

    ?MSGX :=  '管理番号　　＝'  && ?KANRINO
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '作場ＣＤ１　＝'  && %SBSTR(?SAKUCD,1,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ２　＝'  && %SBSTR(?SAKUCD,3,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ３　＝'  && %SBSTR(?SAKUCD,5,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ４　＝'  && %SBSTR(?SAKUCD,7,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ５　＝'  && %SBSTR(?SAKUCD,9,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ６　＝'  && %SBSTR(?SAKUCD,11,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ７　＝'  && %SBSTR(?SAKUCD,13,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ８　＝'  && %SBSTR(?SAKUCD,15,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ９　＝'  && %SBSTR(?SAKUCD,17,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１０＝'  && %SBSTR(?SAKUCD,19,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１１＝'  && %SBSTR(?SAKUCD,21,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１２＝'  && %SBSTR(?SAKUCD,23,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１３＝'  && %SBSTR(?SAKUCD,25,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１４＝'  && %SBSTR(?SAKUCD,27,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１５＝'  && %SBSTR(?SAKUCD,29,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１６＝'  && %SBSTR(?SAKUCD,31,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１７＝'  && %SBSTR(?SAKUCD,33,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１８＝'  && %SBSTR(?SAKUCD,35,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ１９＝'  && %SBSTR(?SAKUCD,37,2)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '作場ＣＤ２０＝'  && %SBSTR(?SAKUCD,39,2)
    SNDMSG    ?MSGX,TO-XCTL

/.##発注確認店舗ワーククリア##./
CLRFILE1:

    ?STEP :=   'CLRFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE   ?MEITXXXF

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注確認店舗ワーク　クリア'
              GOTO ABEND END

/.##発注確認明細ワーククリア##./
CLRFILE2:

    ?STEP :=   'CLRFILE2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE   ?MEISXXXF

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注確認明細ワーク　クリア'
              GOTO ABEND END

/.##発注確認　店舗ワーク作成##./
SSY5180B:

    ?STEP :=   'SSY5180B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-MEITXXXF,TOFILE-?MEITXXXF
    CALL      PGM-SSY5180B.TOKSOLIB,PARA-(?KANRINO,?SAKUCD)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注確認店舗ワーク作成'
              GOTO ABEND END

/.##発注確認明細ワーク作成##./
SSY5190B:

    ?STEP :=   'SSY5190B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-MEITXXX1,TOFILE-?MEITXXX1
    OVRF      FILE-MEISXXX1,TOFILE-?MEISXXX1
    CALL      PGM-SSY5190B.TOKSOLIB,PARA-(?KANRINO,?SAKUCD)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注確認明細ワーク作成'
              GOTO ABEND END

/.##発注確認　ＣＳＶデータ作成##./
SSY5200V:

    ?STEP :=   'SSY5200V'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-MEITXXX1,TOFILE-?MEITXXX1
    OVRF      FILE-NCSTXXXF,TOFILE-?NCSTXXXF
    OVRF      FILE-MEISXXX1,TOFILE-?MEISXXX1
    OVRF      FILE-NCSMXXXF,TOFILE-?NCSMXXXF
    CALL      PGM-SSY5200V.TOKSOLIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注確認ＣＳＶデータ作成'
              GOTO ABEND
    END

FIMPORT1:   /.発注ＣＳＶ（店舗）転送./

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-?NCSTXXXF,TYPE-@FILE,
              PARA-HNCSVDT,UNIT-3,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '発注ＣＳＶ（店舗）データ転送'
              GOTO ABEND
    END

FIMPORT2:   /.発注ＣＳＶ（明細）転送./

    ?STEP :=   'FIMPORT2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-?NCSMXXXF,TYPE-@FILE,
              PARA-HNCSVDT,UNIT-4,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '発注ＣＳＶ（明細）データ転送'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '発注確認データ作成が異常しました。'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

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

    RETURN    PGMEC-?PGMEC

ERR:  /.資源の解放./

    ?OPR4  :=  ?KEKA4
    ?OPR1  :=  '　　＃＃＃＃＃＃＃　資源使用中　＃＃＃＃＃＃＃　　'
    ?OPR2  :=  '　　現在、資料出力用ワークファイルが他の端末で　　'
    ?OPR3  :=  '　　使用中です。確認して下さい。'
/.  ?OPR4  :=  ''
./  ?OPR5  :=  '　　ＥＮＴＥＲ＝再実行，ＰＦ９＝プログラム終了　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    GOTO   FILECHK
