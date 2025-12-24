# PSY3817A

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSY3817A.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    ナフコ出荷支援                       *  ./
/. *   JOB-ID      :    PSY3817A（更新のみ）                 *  ./
/. *   JOB-NAME    :    発注関係資料出力（箱数計算Ｖｅｒ）   *  ./
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
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY3817A'
    VAR       ?STEP     ,STRING*8
    /.端末別ファイルＩＤ編集用./
              /.端末番号./
    VAR       ?TANNO    ,STRING*03,VALUE-'   '
              /.発注書店舗ワーク./
    VAR       ?HACT     ,STRING*4,VALUE-'HACT'
    VAR       ?HACTF    ,STRING*1,VALUE-'F'
    VAR       ?HACT1    ,STRING*1,VALUE-'1'
    VAR       ?HACTXXXF ,NAME!MOD
    VAR       ?HACTXXX1 ,NAME!MOD
              /.発注書商品明細ワーク./
    VAR       ?HACS     ,STRING*4,VALUE-'HACS'
    VAR       ?HACSF    ,STRING*1,VALUE-'F'
    VAR       ?HACS1    ,STRING*1,VALUE-'1'
    VAR       ?HACSXXXF ,NAME!MOD
    VAR       ?HACSXXX1 ,NAME!MOD
              /.発注書印刷ワーク./
    VAR       ?HACP     ,STRING*4,VALUE-'HACP'
    VAR       ?HACPF    ,STRING*1,VALUE-'F'
    VAR       ?HACP1    ,STRING*1,VALUE-'1'
    VAR       ?HACPXXXF ,NAME!MOD
    VAR       ?HACPXXX1 ,NAME!MOD
              /.ワークファイルＩＤ表示用./
    VAR       ?NHACLSTW ,STRING*17,VALUE-'                 '

              /.発注データＣＳＶ（店舗）./
    VAR       ?CSVT     ,STRING*4,VALUE-'CSVT'
    VAR       ?CSVTF    ,STRING*1,VALUE-'F'
    VAR       ?CSVTXXXF ,NAME!MOD
              /.発注データＣＳＶ（商品）./
    VAR       ?CSVS     ,STRING*4,VALUE-'CSVS'
    VAR       ?CSVSF    ,STRING*1,VALUE-'F'
    VAR       ?CSVSXXXF ,NAME!MOD
              /.ＣＳＶファイルＩＤ表示用./
    VAR       ?NHACCSVW ,STRING*17,VALUE-'                 '

              /.梱包数計算ワーク./
    VAR       ?HAKO     ,STRING*4,VALUE-'HAKO'
    VAR       ?HAKOF    ,STRING*1,VALUE-'F'
    VAR       ?HAKO1    ,STRING*1,VALUE-'1'
    VAR       ?HAKOXXXF ,NAME!MOD
    VAR       ?HAKOXXX1 ,NAME!MOD

    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*7,VALUE-'TOKDLIB'
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?KANRINO  ,STRING*8,VALUE-'00000000' /.管理番号    ./
    VAR       ?P1       ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?BTYMD    ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?P2       ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?BTTIME   ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?P3       ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?BTTOKC   ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?P4       ,STRING*2,VALUE-'0'        /.倉庫ＣＤ  ./
    VAR       ?SAKUCD   ,STRING*2,VALUE-'00'       /.作場ＣＤ　./
    VAR       ?SAKUCDS  ,STRING*2,VALUE-'00'       /.作場(開始)./
    VAR       ?SAKUCDE  ,STRING*2,VALUE-'00'       /.作場(終了)./
    VAR       ?P5       ,STRING*1,VALUE-'0'        /.発注／訂正  ./
    VAR       ?P6       ,STRING*1,VALUE-'0'        /.出力順      ./
    VAR       ?SYUPTN   ,STRING*1,VALUE-'0'        /.出力パターン./
    VAR       ?SYURUI   ,STRING*1,VALUE-'0'        /.ONL手書区分./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./
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

    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '発注関係資料出力'

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

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
/.##端末別リスト出力ワークファイルＩＤ取得##./
   /.発注書店舗ワーク./
    ?FILNM    :=    ?HACT && ?TANNO && ?HACTF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HACTXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?HACTXXXF)
    ?MSGX     :=    '##発注書店舗ワーク　　=' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?HACT && ?TANNO && ?HACT1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HACTXXX1 :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?HACTXXX1)
    ?MSGX     :=    '##                     ' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.発注書商品明細ワーク./
    ?FILNM    :=    ?HACS && ?TANNO && ?HACSF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HACSXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?HACSXXXF)
    ?MSGX     :=    '##発注書商品明細ワーク=' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?HACS && ?TANNO && ?HACS1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HACSXXX1 :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?HACSXXX1)
    ?MSGX     :=    '##                     ' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.発注書印刷ワーク./
    ?FILNM    :=    ?HACP && ?TANNO && ?HACPF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HACPXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?HACPXXXF)
    ?MSGX     :=    '##発注書印刷ワーク　　=' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?HACP && ?TANNO && ?HACP1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HACPXXX1 :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?HACPXXX1)
    ?MSGX     :=    '##                     ' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.ＣＳＶ出力用ワーク（店舗）./
    ?FILNM    :=    ?CSVT && ?TANNO && ?CSVTF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CSVTXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACCSVW :=    %STRING(?CSVTXXXF)
    ?MSGX     :=    '##発注ＣＳＶ（店舗）　=' && ?NHACCSVW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.ＣＳＶ出力用ワーク（明細）./
    ?FILNM    :=    ?CSVS && ?TANNO && ?CSVSF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CSVSXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACCSVW :=    %STRING(?CSVSXXXF)
    ?MSGX     :=    '##発注ＣＳＶ（明細）　=' && ?NHACCSVW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.箱数計算ワーク./
    ?FILNM    :=    ?HAKO && ?TANNO && ?HAKOF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HAKOXXXF :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?HAKOXXXF)
    ?MSGX     :=    '##箱数計算ワーク　　　=' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?FILNM    :=    ?HAKO && ?TANNO && ?HAKO1
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HAKOXXX1 :=    %NCAT(?FILID,?LIBID)
    ?NHACLSTW :=    %STRING(?HAKOXXX1)
    ?MSGX     :=    '##                     ' && ?NHACLSTW
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##端末別ファイル排他##./
FILECHK:
   /.発注書店舗ワーク./
    ASSIGN FILE-?HACTXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注書店舗ワーク　排他処理'
           GOTO   ERR
    END
   /.発注書商品明細ワーク./
    ASSIGN FILE-?HACSXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注書店舗ワーク　排他処理'
           GOTO   ERR
    END
   /.発注書印刷ワーク./
    ASSIGN FILE-?HACPXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注書印刷ワーク　排他処理'
           GOTO   ERR
    END
   /.発注ＣＳＶファイル(店舗)./
    ASSIGN FILE-?CSVTXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注ＣＳＶファイル(店舗)排他処理'
           GOTO   ERR
    END

   /.発注ＣＳＶファイル(明細)./
    ASSIGN FILE-?CSVSXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := '発注ＣＳＶファイル(明細)排他処理'
           GOTO   ERR
    END

/.##発注関係資料出力指示入力##./
SSY3816I:

    ?STEP :=   'SSY3816I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    CALL      PGM-SSY3816I.TOKELIBO,PARA-(?KANRINO,?SAKUCD,
                                          ?SYUPTN,?SYURUI)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END

    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注関係資料出力指示'
              GOTO   ABEND
    END

    ?MSGX :=  '管理番号＝'  && ?KANRINO
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '作場ＣＤ＝'  && ?SAKUCD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '出力ﾊﾟﾀｰﾝ='  && '？？？'
    IF        ?SYUPTN = '1'  THEN
              ?MSGX :=  '出力ﾊﾟﾀｰﾝ='  && '在庫確認書'
    END
    IF        ?SYUPTN = '2'  THEN
              ?MSGX :=  '出力ﾊﾟﾀｰﾝ='  && '在庫確定書'
    END
    IF        ?SYUPTN = '3'  THEN
              ?MSGX :=  '出力ﾊﾟﾀｰﾝ='  && '確定前 発注ＣＳＶ'
    END
    IF        ?SYUPTN = '4'  THEN
              ?MSGX :=  '出力ﾊﾟﾀｰﾝ='  && '確定後 発注ＣＳＶ'
    END
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '出力種類='  && '？？？'
    IF        ?SYURUI = '1'  THEN
              ?MSGX :=  '出力種類='  && 'オンライン'
    END
    IF        ?SYURUI = '2'  THEN
              ?MSGX :=  '出力種類='  && '本発／手書'
    END
    SNDMSG    ?MSGX,TO-XCTL

/.##発注書店舗ワーククリア##./
CLRFILE1:

    ?STEP :=   'CLRFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE   ?HACTXXXF

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注書店舗ワーク　クリア'
              GOTO ABEND END

/.##発注書商品明細ワーククリア##./
CLRFILE2:

    ?STEP :=   'CLRFILE2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE   ?HACSXXXF

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注書商品明細ワーク　クリア'
              GOTO ABEND END

/.##発注書印刷ワーククリア##./
CLRFILE3:

    ?STEP :=   'CLRFILE3'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE   ?HACPXXXF

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注書印刷ワーク　クリア'
              GOTO ABEND END

/.##箱数計算ワーククリア##./
CLRFILE4:

    ?STEP :=   'CLRFILE4'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CLRFILE   ?HAKOXXXF

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '箱数計算ワーク　クリア'
              GOTO ABEND END

    IF        ?SYUPTN    =  '2'   THEN  /.##在庫確定書##./
              GOTO SSY3818K
    END
    IF        ?SYUPTN    =  '4'   THEN  /.##ＣＳＶ確定後##./
              GOTO SSY3818K
    END

/.##発注関係資料　データ抽出##./
SSY3817B:

    ?STEP :=   'SSY3817B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CASE  ?SYURUI OF
       /.オンライン./
       #'1'#  OVRF FILE-NFJOHOL1,TOFILE-NFJOHOL1.TOKDLIB
       /.本発　　　./
       #'2'#  OVRF FILE-NFJOHOL1,TOFILE-NHJOHOL1.TOKDLIB
    END
    OVRF FILE-HAKOXXX1,TOFILE-?HAKOXXX1
    CALL      PGM-SSY3817K.TOKELIBO,PARA-(?KANRINO,?SAKUCD)
                                     /.   ?SYUPTN,?SYURUI) ./
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注関係資料　データ抽出'
              GOTO ABEND END

/.##発注書店舗ワーク作成##./
SSY3818K:

    ?STEP :=   'SSY3818K'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-HCTPXXXF,TOFILE-?HACTXXXF
    CALL      PGM-SSY3818K.TOKELIBO,PARA-(?KANRINO,?SAKUCD)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注書店舗ワーク作成'
              GOTO ABEND END

/.##発注書商品明細ワーク作成##./
SSY3819K:

    ?STEP :=   'SSY3819K'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-HCTPXXX1,TOFILE-?HACTXXX1
    OVRF      FILE-HCSMXXX1,TOFILE-?HACSXXX1
    CALL      PGM-SSY3819K.TOKELIBO,PARA-(?KANRINO,?SAKUCD)

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注書商品明細ワーク作成'
              GOTO ABEND END

BUNKI:
    CASE  ?SYUPTN OF
       #'1'#  GOTO  LISTOUT     /.在庫確認書./
       #'2'#  GOTO  LISTOUT     /.在庫確定書./
       #'3'#  GOTO  CSVOUT      /.確定前 発注CSV出力./
       #'4'#  GOTO  CSVOUT      /.確定後 発注CSV出力./
    END

/.##在庫確認書OR在庫確定書##./
LISTOUT:

SSY3820B:  /.##発注書印刷ワーク作成##./

    ?STEP :=   'SSY3820B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-HCTPXXX1,TOFILE-?HACTXXX1
    OVRF      FILE-HCSMXXX1,TOFILE-?HACSXXX1
    OVRF      FILE-HCPRXXX1,TOFILE-?HACPXXX1
    CALL      PGM-SSY3820B.TOKELIBO

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注書印刷ワーク作成'
              GOTO ABEND END

SSY3822L:   /.在庫確認書/在庫確定書./

    ?STEP :=   'SSY3822L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

/.  DLTOVRF
./  OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHKL,
    OUTQ-XXPAMGQL,MEDLIB-TOKELIBO
    OVRF      FILE-HCSMXXX1,TOFILE-?HACSXXX1
/.  CALL      PGM-SSY3822K.TOKELIBO,PARA-(?SYUPTN,?BUMON,?TANCD)
./  ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '在庫確認書/確定書　発行'
              GOTO ABEND
    END

SSY3821L:   /.発注書./

    ?STEP :=   'SSY3821L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHKL,
    OUTQ-XXPAMGQL,MEDLIB-TOKELIBO
    OVRF      FILE-HCPRXXX1,TOFILE-?HACPXXX1
/.  CALL      PGM-SSY3821C.TOKELIBO,PARA-(?BUMON,?TANCD)
./  ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注書　発行'
              GOTO ABEND
    END

/.終了へ./
    GOTO      RTN


/.##発注ＣＳＶ出力##./
CSVOUT:

SSY3823V:   /.発注ＣＳＶ出力./

    ?STEP :=   'SSY3823V'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-HCTPXXX1,TOFILE-?HACTXXX1
    OVRF      FILE-HCCSVTEN,TOFILE-?CSVTXXXF
    OVRF      FILE-HCSMXXX1,TOFILE-?HACSXXX1
    OVRF      FILE-HCCSVMEI,TOFILE-?CSVSXXXF
/.  CALL      PGM-SSY3823V.TOKELIBO
./  ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '発注ＣＳＶ　出力'
              GOTO ABEND
    END

FIMPORT1:   /.発注ＣＳＶ（店舗）転送./

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  FIMPORT   FILE-?CSVTXXXF,TYPE-@FILE,
              PARA-NFHACCSK,UNIT-1,OPR-@NO
./  ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '発注ＣＳＶ（店舗）データ転送'
              GOTO ABEND
    END

FIMPORT2:   /.発注ＣＳＶ（明細）転送./

    ?STEP :=   'FIMPORT2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  FIMPORT   FILE-?CSVSXXXF,TYPE-@FILE,
              PARA-NFHACCSK,UNIT-2,OPR-@NO
./  ?PGMEC    :=    @PGMEC
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
    ?KEKA1 :=  '発注関係資料出力が異常しました。'
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

    ?OPR1  :=  '　　＃＃＃＃＃＃＃　資源使用中　＃＃＃＃＃＃＃　　'
    ?OPR2  :=  '　　現在、資料出力用ワークファイルが他の端末で　　'
    ?OPR3  :=  '　　使用中です。確認して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  '　　ＥＮＴＥＲ＝再実行，ＰＦ９＝プログラム終了　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    GOTO   FILECHK

```
