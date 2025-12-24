/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    ナフコ出荷支援                       *  ./
/. *   JOB-ID      :    PSY38130                             *  ./
/. *   JOB-NAME    :    オンライン作場振分情報リスト出力     *  ./
/. *   UPDATE      :    2016/08/22                           *  ./
/. *                      本発ＥＤＩ対応                     *  ./
/. *   UPDATE      :    2018/07/20                           *  ./
/. *                      本社統合対応　　                   *  ./
/. ***********************************************************  ./
    PGM   (P1-?PAR1,P2-?PAR2,P3-?PAR3)

    PARA ?PAR1  ,STRING*8,IN,VALUE-'00000000'
    PARA ?PAR2  ,STRING*4,IN,VALUE-'0000'
    PARA ?PAR3  ,STRING*8,IN,VALUE-'00000000'

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSY38130'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?P1       ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?BTYMD    ,STRING*8,VALUE-'00000000' /.受信日付    ./
    VAR       ?P2       ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?BTTIME   ,STRING*4,VALUE-'0000'     /.受信時間    ./
    VAR       ?P3       ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?BTTOKC   ,STRING*8,VALUE-'00000000' /.受信取引先  ./
    VAR       ?P4       ,STRING*2,VALUE-'00'       /.倉庫ＣＤ  ./
    VAR       ?SAKUCDS  ,STRING*2,VALUE-'00'       /.作場(開始)./
    VAR       ?SAKUCDE  ,STRING*2,VALUE-'00'       /.作場(終了)./
  /.VAR       ?P5       ,STRING*1,VALUE-'0' ./     /.発注／訂正  ./
  /.VAR       ?P6       ,STRING*1,VALUE-'0' ./     /.出力順      ./
    VAR       ?SYUKBN   ,STRING*1,VALUE-'0'        /.出力順      ./
    VAR       ?P7       ,STRING*2,VALUE-'00'       /.代表倉庫    ./
  /.↓2016/08/22 ./
    VAR       ?SYOBUNS ,STRING*8,VALUE-'        ' /.商品分類(開始./
    VAR       ?SYOBUNE ,STRING*8,VALUE-'        ' /.商品分類(終了./
    VAR       ?PATNS   ,STRING*2,VALUE-'  '       /.ﾊﾟﾀｰﾝCD(開始./
    VAR       ?PATNE   ,STRING*2,VALUE-'  '       /.ﾊﾟﾀｰﾝCD(終了./
    VAR       ?HAKKOU  ,STRING*1,VALUE-' '        /.発行区分　　./
    VAR       ?OUTPUT  ,STRING*1,VALUE-' '        /.出力区分　　./
  /.↑2016/08/22 ./
  /.↓2016/09/15 ./
    VAR       ?SYKKBN  ,STRING*1,VALUE-' '        /.集荷区分　　./
    VAR       ?NOUHDT  ,STRING*8,VALUE-'        ' /.納品日　　　./
  /.↑2016/09/15 ./
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
/.-----------------------------------------------------------./
  /.↓2016/08/22 ./
    /.端末別ファイルＩＤ編集用./
           /.端末番号./
    VAR       ?TANNO    ,STRING*03,VALUE-'   '
           /.振分リストワーク(商品順)./
    VAR       ?HUR      ,STRING*3,VALUE-'HUR'
    VAR       ?HURW     ,STRING*1,VALUE-'W'
    VAR       ?HURXXXW  ,NAME!MOD
           /.振分リストワーク(店舗順)./
    VAR       ?HURWT    ,STRING*2,VALUE-'WT'
    VAR       ?HURXXXWT ,NAME!MOD

           /.作場振分リストＣＳＶ./
    VAR       ?CSVH     ,STRING*4,VALUE-'CSVH'
    VAR       ?CSVHF    ,STRING*1,VALUE-'F'
    VAR       ?CSVHXXXF ,NAME!MOD
           /.作場振分店舗ＣＳＶ./
    VAR       ?CSVHT    ,STRING*1,VALUE-'T'
    VAR       ?CSVHXXXT ,NAME!MOD

           /.ファイルＩＤ表示用./
    VAR       ?FILENAME ,STRING*17,VALUE-'                 '

    VAR       ?FILNM    ,STRING*8,VALUE-'        '
    VAR       ?LIBNM    ,STRING*7,VALUE-'TOKDLIB'
    VAR       ?FILID    ,NAME
    VAR       ?LIBID    ,NAME
  /.↑2016/08/22 ./

/.----------------------------------------------------------------./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'オンライン作場振分情報リスト出力'

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
   /.振分リストワーク(店舗順)./
    ?FILNM    :=    ?HUR && ?TANNO && ?HURWT
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HURXXXWT :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?HURXXXWT)
    ?MSGX     :=    '##振分ﾘｽﾄﾜｰｸ(店舗)　=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.振分リストワーク(商品順)./
    ?FILNM    :=    ?HUR && ?TANNO && ?HURW
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?HURXXXW  :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?HURXXXW)
    ?MSGX     :=    '##振分ﾘｽﾄﾜｰｸ(商品)　=' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.作場振分リストＣＳＶ./
    ?FILNM    :=    ?CSVH && ?TANNO && ?CSVHF
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CSVHXXXF :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?CSVHXXXF)
    ?MSGX     :=    '##作場振分リストCSV =' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   /.作場振分店舗ＣＳＶ./
    ?FILNM    :=    ?CSVH && ?TANNO && ?CSVHT
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?CSVHXXXT :=    %NCAT(?FILID,?LIBID)
    ?FILENAME :=    %STRING(?CSVHXXXT)
    ?MSGX     :=    '##作場振分店舗CSV　 =' && ?FILENAME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

FILECHK:

    ASSIGN FILE-?HURXXXWT!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得１'
           GOTO   ERR
    END

    ASSIGN FILE-?HURXXXW!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得２'
           GOTO   ERR
    END

    ASSIGN FILE-?CSVHXXXF!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得３'
           GOTO   ERR
    END

    ASSIGN FILE-?CSVHXXXT!@XCL
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF     ?PGMEC  ^=    0    THEN
           ?KEKA4 := 'ファイル排他獲得４'
           GOTO   ERR
    END

/.----------------------------------------------------------------./
/.##作場振分情報リスト発行指示入力##./
SSY3813I:

    ?STEP :=   'SSY3813I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKELIBO/TOKDLIB/TOKFLIB/TOKKLIB

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
  /.↓2016/08/22./
  /.CALL      PGM-SSY3813I.TOKELIBO,PARA-(?BTYMD,?BTTIME,?BTTOKC,
                                          ?SAKUCDS,?SAKUCDE,?SYUKBN)
  ./
    CALL      PGM-SSY3813I.TOKELIBO,PARA-(?BTYMD,?BTTIME,?BTTOKC,
                                          ?SAKUCDS,?SAKUCDE,?SYUKBN,
                                          ?SYOBUNS,?SYOBUNE,
                                          ?PATNS,?PATNE,
                                      /.  ?HAKKOU,?OUTPUT) ./
                                          ?HAKKOU,?OUTPUT,
                                          ?SYKKBN,?NOUHDT)
  /.↑2016/08/22./
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO   RTN
    END
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '作場振分情報リスト発行指示'
              GOTO   ABEND
    END

    ?MSGX :=  '受信日　＝'  && ?BTYMD
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '受信時刻　　＝'  && ?BTTIME
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '取引先　　　＝'  && ?BTTOKC
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '開始作場　　＝'  && ?SAKUCDS
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '終了作場　　＝'  && ?SAKUCDE
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '出力順　　　＝'  && '？？？'
    IF        ?SYUKBN = '1'  THEN
              ?MSGX :=  '出力順　　　＝'  && '相手商品ＣＤ順'
    END
    IF        ?SYUKBN = '2'  THEN
              ?MSGX :=  '出力順　　　＝'  && '自社商品ＣＤ順'
    END
    SNDMSG    ?MSGX,TO-XCTL

    /.↓2016/08/22./
    ?MSGX :=  '開始商品分類＝'  && ?SYOBUNS
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '終了商品分類＝'  && ?SYOBUNE
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '開始パターン＝'  && ?PATNS
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '終了パターン＝'  && ?PATNE
    SNDMSG    ?MSGX,TO-XCTL

    ?MSGX :=  '発行区分　　＝'  && '？？？'
    IF        ?HAKKOU = ' '  THEN
              ?MSGX :=  '発行区分　　＝'  && '帳票出力'
              SNDMSG    ?MSGX,TO-XCTL

              ?MSGX :=  '出力区分　　＝'  && '？？？'
              IF        ?OUTPUT = ' '  THEN
                        ?MSGX :=  '出力区分　　＝'  && 'レーザー'
              END
              IF        ?OUTPUT = '1'  THEN
                        ?MSGX :=  '出力区分　　＝'  && '連帳'
              END
              SNDMSG    ?MSGX,TO-XCTL
    END
    IF        ?HAKKOU = '1'  THEN
              ?MSGX :=  '発行区分　　＝'  && 'ＣＳＶ出力'
              SNDMSG    ?MSGX,TO-XCTL
    END
    /.↑2016/08/22./

/.##作場振分情報リストデータ抽出##./
SSY3814B:

    ?STEP :=   'SSY3814B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
  /.↓2016/08/22./
  /.CALL      PGM-SSY3814B.TOKELIBO,PARA-(?BTYMD,?BTTIME,?BTTOKC,
                                          ?SAKUCDS,?SAKUCDE)
  ./
    OVRF FILE-HURXXXW,TOFILE-?HURXXXW
    CALL      PGM-SSY3814B.TOKELIBO,PARA-(?BTYMD,?BTTIME,?BTTOKC,
                                          ?SAKUCDS,?SAKUCDE,
                                          ?SYOBUNS,?SYOBUNE,
                                       /. ?PATNS,?PATNE) ./
                                          ?PATNS,?PATNE,
                                          ?NOUHDT,?SYKKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '作場振分情報リストデータ抽出'
              GOTO ABEND END

    IF        ?SYUKBN    =  '1'   THEN  /.##相手商品ｺｰﾄﾞ順##./
              GOTO AITESCD
    END

    IF        ?SYUKBN    =  '2'   THEN  /.##自社商品ｺｰﾄﾞ順##./
              GOTO JISYASCD
    END

/.----------------------------------------------------------------./
/.##相手商品ｺｰﾄﾞ順#./
AITESCD:
/.##ＳＯＲＴ　相手商品コード##./
SORT1:

/.↓2016/08/22./
/.ＳＯＲＴ順：倉庫・部門・ルート・納品日・相手商品ｺｰﾄﾞ・店舗
                            ↓
  ＳＯＲＴ順：倉庫・部門・商品分類ＣＤ・納品日・相手商品ｺｰﾄﾞ・
  　　　　　　店舗・パターンＣＤ./
/.↑2016/08/22./
    ?STEP :=  'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.↓2016/08/22./
  /.SORT      INFILE-HURLSTWK.TOKDLIB,INRL-256,INBF-15,
              OUTFILE-HURLSTWK.TOKDLIB,OUTBF-15,
              KEY-26!2!CA,KEY1-28!4!CA,KEY2-183!10!CA,KEY3-40!8!CA,
              KEY4-48!13!CA,KEY5-21!5!CA,
              RCDL-@DSP
  ./
    SORT      INFILE-?HURXXXW,INRL-256,INBF-15,
              OUTFILE-?HURXXXW,OUTBF-15,
              KEY-26!2!CA,KEY1-28!4!CA,KEY2-208!8!CA,KEY3-40!8!CA,
              KEY4-48!13!CA,KEY5-216!2!CA,KEY6-21!5!CA,
              RCDL-@DSP

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＳＯＲＴ　相手商品コード'
              GOTO ABEND
    ELSE
        /.    GOTO SSY3815L   2016/08/22./
              GOTO BUNKI
    END

/.##自社商品ｺｰﾄﾞ順#./
JISYASCD:
/.##ＳＯＲＴ　サカタ商品コード順##./
SORT2:
/.↓2016/08/22./
/.ＳＯＲＴ順：倉庫・部門・ルート・納品日・自社商品ｺｰﾄﾞ・店舗
                            ↓
/.ＳＯＲＴ順：倉庫・部門・商品分類ＣＤ・納品日・自社商品ｺｰﾄﾞ・
  　　　　　　店舗・パターンＣＤ./
/.↑2016/08/22./
    ?STEP :=  'SORT2   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.↓2016/08/22./
  /.SORT      INFILE-HURLSTWK.TOKDLIB,INRL-256,INBF-15,
              OUTFILE-HURLSTWK.TOKDLIB,OUTBF-15,
              KEY-26!2!CA,KEY1-28!4!CA,KEY2-183!10!CA,KEY3-40!8!CA,
              KEY4-61!16!CA,KEY5-21!5!CA,
              RCDL-@DSP
      ./
    SORT      INFILE-?HURXXXW,INRL-256,INBF-15,
              OUTFILE-?HURXXXW,OUTBF-15,
              KEY-26!2!CA,KEY1-28!4!CA,KEY2-208!8!CA,KEY3-40!8!CA,
              KEY4-61!16!CA,KEY5-216!2!CA,KEY6-21!5!CA,
              RCDL-@DSP

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＳＯＲＴ　自社商品コード'
              GOTO ABEND
    ELSE
        /.    GOTO SSY3815L   2016/08/22./
              GOTO BUNKI
    END

/.----------------------------------------------------------------./
/.##処理分岐## 2016/08/22 ./
BUNKI:

    CASE      ?HAKKOU   OF
       /.帳票./
       #' '#  GOTO      SSY3815L
       /.ＣＳＶ./
       #'1'#  GOTO      SORT3
    END

/.----------------------------------------------------------------./
/.##オンライン作場振分情報リスト（作表）##./
SSY3815L:

    ?STEP :=   'SSY3815L'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

  /.↓2016/08/22./
  /.OVRPRTF FILE-XU04LP,TOFILE-AMAGILBP.XUCL  ./
    ?MSGX :=  'P4 = '  && ?P4
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    CASE      ?OUTPUT   OF
    /.レーザー./                                 /.↓社内にない./
    /.現地./   #' '#  IF  ?P4  =  '01'  THEN
                          ?MSGX :=  '出力先 = 本社'
                          SNDMSG  ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                          OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
                      ELSE
                          ?MSGX :=  '出力先 = 九州'
                          SNDMSG  ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                          OVRPRTF FILE-XU04LP,TOFILE-AMAGILBP.XUCL
                      END

    /.社内./ /.#' '#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL./

    /.連帳./                           /.↓現地はXU04LPでOK？./
       #'1'#  OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    END
  /.↑2016/08/22./

    CALL      PGM-SSY3815L.TOKELIBO,PARA-(?SYUKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'オンライン作場振分情報リスト'
              GOTO ABEND
    END
    GOTO      RTN

/.----------------------------------------------------------------./
/.##ＳＯＲＴ　振分リストワーク(店舗順)##./
SORT3:
/.ＳＯＲＴ順：店舗./

    ?STEP :=  'SORT3   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    SORT      INFILE-?HURXXXW,INRL-256,INBF-15,
              OUTFILE-?HURXXXWT,OUTBF-15,
              KEY-21!5!CA,
              RCDL-@DSP

    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := 'ＳＯＲＴ　店舗コード'
              GOTO ABEND
    END


/.##オンライン作場振分情報ＣＳＶ作成##./
SSY3910V:

    ?STEP :=   'SSY3910V'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-HURXXXWT,TOFILE-?HURXXXWT
    OVRF      FILE-HURXXXW,TOFILE-?HURXXXW
    OVRF      FILE-CSVHXXXF,TOFILE-?CSVHXXXF
    OVRF      FILE-CSVHXXXT,TOFILE-?CSVHXXXT
    CALL      PGM-SSY3910V.TOKELIBO,PARA-(?SYUKBN)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 := '作場振分情報ＣＳＶ作成'
              GOTO ABEND
    END

FIMPORT1:   /.作場振分リストＣＳＶ転送./

    ?STEP :=   'FIMPORT1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-?CSVHXXXF,TYPE-@FILE,
              PARA-NFHACCSK,UNIT-3,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '作場振分リストＣＳＶ転送'
              GOTO ABEND
    END

FIMPORT2:   /.作場振分店舗ＣＳＶ転送./

    ?STEP :=   'FIMPORT2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-?CSVHXXXT,TYPE-@FILE,
              PARA-NFHACCSK,UNIT-4,OPR-@NO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '作場振分店舗ＣＳＶ転送'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '作場振分情報リスト出力が異常しました。'
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
    ?OPR2  :=  '　　現在、振分リストワークファイルを他で使用中　　'
    ?OPR3  :=  '　　です。少し時間をおいて再度、実行して下さい。　'
    ?OPR4  :=  ''
    ?OPR5  :=  '　　ＥＮＴＥＲ＝再実行，ＰＦ９＝プログラム終了　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    GOTO   FILECHK
