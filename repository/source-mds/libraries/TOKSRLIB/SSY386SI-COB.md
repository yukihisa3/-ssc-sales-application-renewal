# SSY386SI

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY386SI.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援システム　　        *
*    モジュール名　　　　：　ナフコ商品マスタ保守（資材用）    *
*    作成日／作成者　　　：　15/05/08 NAV TAKAHASHI            *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　ナフコ商品マスタの登録、修正、削　*
*                            除を行なう。　                    *
*    更新日／更新者　　　：　16/08/19 NAV TAKAHASHI            *
*    変更概要　　　　　　：　商品分類ＣＤの追加　　　　　　　　*
*    更新日／更新者　　　：　19/12/05 ASS TAKAHASHI            *
*    変更概要　　　　　　：　出荷場所・棚番・仕単・取Ｃの追加　*
*    　　　　　　　　　　：　商品変換テーブルの更新機能を追加　*
*    更新日／更新者　　　：　21/03/01 NAV INOUE                *
*    変更概要　　　　　　：　メンテリスト出力に対応　　　　　　*
*    　　　　　　　　　　：　TOKSLIBS→TOKSRLIBに移動
*    更新日／更新者　　　：　21/12/31 NAV INOUE                *
*    変更概要　　　　　　：　ＳＵＢ商品変換マスタ更新追加　　　*
*    　　　　　　　　　　：　
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY386SI.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
*---<<  ナフコ商品マスタ  >>---*
     SELECT   NFSHOMS    ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                        FILE      STATUS    IS   SHO-STATUS.
*---<<  商品変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TBL-F01
                                                 TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*---<<  担当者マスタ  >>---*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TAN-F01
                                                 TAN-F02
                        FILE      STATUS    IS   TAN-STATUS.
*---<<  作場マスタ　  >>---*
     SELECT   SAKUBAF   ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SAK-F01
                        FILE      STATUS    IS   SAK-STATUS.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*##2019/12/05 INSERT START
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*##2019/12/05 END
*
*##2020/03/01 START
     SELECT   HSHOTBR   ASSIGN    TO        DA-01-VI-HSHOTBR1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   STR-F01
                                                 STR-F02
                                                 STR-F03
                        FILE      STATUS    IS   STR-STATUS.
*##2020/03/01 END
*
*↓2021/12/21
*---<<  サブ商品変換テーブル  >>---*
     SELECT   SUBTBLF    ASSIGN    TO        DA-01-VI-SUBTBLL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SUB-F01
                                                 SUB-F02
                        FILE      STATUS    IS   SUB-STATUS.
*---<<  サブ商品名称マスタ  >>---*
     SELECT   SUBMEIF   ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SBM-F011
                                                 SBM-F0121
                                                 SBM-F0122
                                                 SBM-F0123
                        FILE      STATUS    IS   SBM-STATUS.
*↑2021/12/21
*
*--------------------------------------------------------------
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
     COPY     FSY386S1  OF        XMDLIB
              JOINING   DSP       PREFIX.
*---<<  ナフコ商品マスタ  >>---*
 FD  NFSHOMS.
**     COPY     NFSHOMS
**              DISJOINING XXX JOINING SHO AS PREFIX.
     COPY     NFSHOMS    OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  HSHOTBL.
**     COPY     HSHOTBL
**              DISJOINING XXX JOINING TBL AS PREFIX.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*---<<  商品名称マスタ　  >>---*
 FD  HMEIMS.
**     COPY     HMEIMS
**              DISJOINING XXX JOINING MEI AS PREFIX.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  担当者マスタ　　  >>---*
 FD  HTANMS.
**     COPY     HTANMS
**              DISJOINING XXX JOINING TAN AS PREFIX.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*---<<  条件ファイル　　  >>---*
 FD HJYOKEN.
**     COPY     HJYOKEN
**              DISJOINING XXX JOINING JYO AS PREFIX.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  作場マスタ        >>---*
 FD  SAKUBAF.
**     COPY     SAKUBAF
**              DISJOINING XXX JOINING SAK AS PREFIX.
     COPY     SAKUBAF   OF        XFDLIB
              JOINING   SAK       PREFIX.
*##2019/12/05 INSERT START
*---<<  倉庫マスタ        >>---*
 FD  ZSOKMS.
**     COPY     ZSOKMS
**              DISJOINING XXX JOINING SOK AS PREFIX.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*##2019/12/05 END
*
*##2021/03/01 INSERT START
***## < 商品変換テーブル履歴ファイル >
 FD  HSHOTBR
     LABEL       RECORD    IS        STANDARD.
     COPY        HSHOTBR   OF        XFDLIB
     JOINING     STR       AS        PREFIX.
*##2021/03/01 END
*
*↓2021/12/21
*---<<  サブ商品変換ＴＢＬ　>>---*
 FD  SUBTBLF.
     COPY     SUBTBLF    OF        XFDLIB
              JOINING   SUB       PREFIX.
*---<<  サブ商品名称マスタ　  >>---*
 FD  SUBMEIF.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   SBM       PREFIX.
*↑2021/12/21
*
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ***
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  SHO-STATUS          PIC  X(02).
     02  TBL-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  TAN-STATUS          PIC  X(02).
     02  SAK-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  STR-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
*↓2021/12/21
     02  SUB-STATUS          PIC  X(02).
     02  SBM-STATUS          PIC  X(02).
*↑2021/12/21
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  NFSHOMS-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HTANMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  SAKUBAF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  HJYOKEN-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  ZSOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     02  HSHOTBR-INV-FLG     PIC  X(03)  VALUE SPACE.
*↓2021/12/21
     02  SUBTBLF-INV-FLG     PIC  X(03)  VALUE SPACE.
     02  SUBMEIF-INV-FLG     PIC  X(03)  VALUE SPACE.
*↑2021/12/21
 01  WK-AREA.
     02  WK-SYORI            PIC  9(01)  VALUE ZERO.
     02  WK-SYORI-1          PIC  9(01)  VALUE ZERO.
     02  WK-INI-BUMNCD       PIC  X(04).
     02  IX                  PIC  9(02)  VALUE ZERO.
     02  IY                  PIC  9(02)  VALUE ZERO.
     02  ERR-KBN             PIC  X(03)  VALUE SPACE.
 01  WK-TBL-AREC             PIC  X(85).
 01  WK-DEL-SW               PIC  9(01) VALUE 0.
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  MSG-AREA.
     02  PMSG01            PIC N(20) VALUE
                           NC"_取消".
     02  PMSG02            PIC N(20) VALUE
                           NC"_取消　_再入力".
     02  PMSG03            PIC N(20) VALUE
                           NC"_終了".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY386SI".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(28)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(28)  VALUE
            NC"既に登録済です。".
     02  MSG-ERR3            PIC  N(28)  VALUE
            NC"サカタ商品ＣＤを入力して下さい。".
     02  MSG-ERR4            PIC  N(28)  VALUE
            NC"商品名称マスタに未登録です。".
     02  MSG-ERR5            PIC  N(28) VALUE
            NC"ＪＡＮＣＤを入力して下さい。".
     02  MSG-ERR6            PIC  N(28) VALUE
            NC"原価単価を入力して下さい。".
     02  MSG-ERR7            PIC  N(28) VALUE
            NC"売価単価を入力して下さい。".
     02  MSG-ERR8            PIC  N(28)  VALUE
            NC"原価単価が売価単価を超えています。".
     02  MSG-ERR9            PIC  N(28)  VALUE
            NC"（ランク）原価単価が売価単価を超えています。".
     02  MSG-ERR10           PIC  N(28)  VALUE
            NC"ナフコ商品ＣＤが未登録です。".
     02  MSG-ERR11           PIC  N(28)  VALUE
            NC"処理区分が違います。".
     02  MSG-ERR12           PIC  N(28)  VALUE
            NC"商品変換ＴＢＬ未登録です。".
     02  MSG-ERR13           PIC  N(28)  VALUE
            NC"商品名称マスタ未登録です。".
     02  MSG-ERR14           PIC  N(28)  VALUE
            NC"商品名を入力して下さい".
     02  MSG-ERR15           PIC  N(28)  VALUE
            NC"商品名カナを入力して下さい。".
     02  MSG-ERR16           PIC  N(28)  VALUE
            NC"部門を入力して下さい。".
     02  MSG-ERR17           PIC  N(28)  VALUE
            NC"標準作場ＣＤを入力して下さい。".
     02  MSG-ERR18           PIC  N(28)  VALUE
            NC"作場Ｍ未登録です。".
     02  MSG-ERR19           PIC  N(28)  VALUE
            NC"エリアＣＤ未登録です。".
     02  MSG-ERR20           PIC  N(28)  VALUE
            NC"条件Ｆ未登録です。".
     02  MSG-ERR21           PIC  N(28)  VALUE
            NC"エリアＣＤ、作場ＣＤは必ず入力して下さい。".
     02  MSG-ERR22           PIC  N(28)  VALUE
         NC"エリア作場ＴＢＬは重複不可です。確認して下さい。".
     02  MSG-ERR23           PIC  N(28)  VALUE
         NC"Ｙを指定して下さい。".
     02  MSG-ERR24           PIC  N(28)  VALUE
     NC"箱単数は０以外を入力して下さい。例：１，２，３，４".
*##2016/08/19 NAV ST
     02  MSG-ERR25           PIC  N(28)  VALUE
     NC"商品分類ＣＤを入力して下さい。".
     02  MSG-ERR26           PIC  N(28)  VALUE
     NC"商品分類ＣＤが条件Ｆに未登録です。".
*##2016/08/19 NAV ED
*##2019/12/05 INSERT START
     02  MSG-ERR27           PIC  N(28)  VALUE
     NC"出荷場所ＣＤを入力して下さい。".
     02  MSG-ERR28           PIC  N(28)  VALUE
     NC"出荷場所ＣＤが倉庫マスタに未登録です。".
     02  MSG-ERR29           PIC  N(28)  VALUE
     NC"商品変換ＴＢＬが登録済です。".
*##2019/12/05 END
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(28)
*##2016/08/19 NAV ST
*****************************OCCURS  24   TIMES.
*##2019/12/05 UPDATE START
*****************************OCCURS  26   TIMES.
                             OCCURS  29   TIMES.
*##2019/12/05 END
*##2016/08/19 NAV ED
*エリア＋作場テーブルチェック用
 01  AREA-TBL-CHK.
     02  AREA-TBL-WK-R       OCCURS  10   TIMES.
         03  AREA-TBLCD      PIC  X(01).
 01  AREA-TBL-WK             PIC  X(01).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*レコード退避エリア
 01  DATA-TAIHI            PIC  X(200).
*
 01  SEQ                   PIC  9(02).
*
****  リンク領域  ***
 LINKAGE               SECTION.
 01  PARA-BUMONCDI          PIC  X(04).
 01  PARA-TANCDI            PIC  X(08).
 01  PARA-TOKCDI            PIC  X(08).
 01  PARA-NDATE             PIC  9(08).
 01  PARA-NTIME             PIC  9(04).
*
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION  USING
                                             PARA-BUMONCDI
                                             PARA-TANCDI
                                             PARA-TOKCDI
                                             PARA-NDATE
                                             PARA-NTIME.
*
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      DSPF.
     MOVE     "DSPF    "     TO   ERR-FL-ID.
     MOVE     DSP-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
*    STOP     RUN.
**
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    NFSHOMS.
     MOVE     "NFSHOMS1"      TO   ERR-FL-ID.
     MOVE     SHO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*
**
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE     "SHOTBL1"      TO   ERR-FL-ID.
     MOVE     TBL-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "MEIMS1"       TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTANMS.
     MOVE     "TANMS1"       TO   ERR-FL-ID.
     MOVE     TAN-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC6                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SAKUBAF.
     MOVE     "SAKUBAL1"     TO   ERR-FL-ID.
     MOVE     SAK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC7                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "JYOKEN1"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
**
*
 FILEERR-SEC8                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZSOKMS.
     MOVE     "ZSOKMS1"      TO   ERR-FL-ID.
     MOVE     SOK-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERR-SEC9                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBR.
     MOVE     "HSHOTBR"      TO   ERR-FL-ID.
     MOVE     STR-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERR-SEC9                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SUBMEIF.
     MOVE     "SUBMEIL1"     TO   ERR-FL-ID.
     MOVE     SBM-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERR-SEC10               SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    SUBTBLF.
     MOVE     "SUBTBLL1"     TO   ERR-FL-ID.
     MOVE     SUB-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END     DECLARATIVES.
****************************************************************
 PROGRAM-START               SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROGRAM-END.
     EXIT.
****************************************************************
*    1.0  初期処理                                             *
****************************************************************
 INIT-SEC                    SECTION.
*    システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM  DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.
*    画面表示日付編集
     MOVE  SYS-DATE(1:4)    TO  HEN-DATE-YYYY.
     MOVE  SYS-DATE(5:2)    TO  HEN-DATE-MM.
     MOVE  SYS-DATE(7:2)    TO  HEN-DATE-DD.
*    システム日付取得
     ACCEPT  WK-TIME  FROM TIME.
*    画面表示時刻編集
     MOVE  WK-TIME(1:2)     TO  HEN-TIME-HH.
     MOVE  WK-TIME(3:2)     TO  HEN-TIME-MM.
     MOVE  WK-TIME(5:2)     TO  HEN-TIME-SS.
*ファイルオープン
     OPEN  I-O    DSPF.
     OPEN  I-O    NFSHOMS.
     OPEN  I-O    HSHOTBL.
     OPEN  INPUT           HMEIMS  HTANMS  SAKUBAF  HJYOKEN.
     OPEN  INPUT  ZSOKMS.
     OPEN  I-O    HSHOTBR.
*↓2021.12.21
     OPEN  INPUT  SUBMEIF.
     OPEN  I-O    SUBTBLF.
*↑2021.12.21

     MOVE  "FSY386S1"        TO  DSP-FORMAT.
     MOVE  SPACE            TO  DSP-FSY386S1.
     MOVE  SPACE            TO  END-FLG.
     MOVE  "1"              TO  MAIN-FLG.
     MOVE  2                TO  WK-SYORI.
     MOVE  SPACE            TO  DSP-PROC.
 INIT-END.
     EXIT.
****************************************************************
*    2.0  メイン処理                                           *
****************************************************************
 MAIN-SEC                    SECTION.
     EVALUATE  MAIN-FLG
       WHEN  "1"  PERFORM  SYORI-SUB
       WHEN  "2"  PERFORM  HEAD-SUB
       WHEN  "3"  PERFORM  BODY-SUB
       WHEN  "4"  PERFORM  BODY2-SUB
       WHEN  "5"  PERFORM  KAKUNIN-SUB
       WHEN  "6"  PERFORM  FILPRT-SUB
       WHEN  OTHER  CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*--------------------------------------------------------------*
*    処理区分入力                                              *
*--------------------------------------------------------------*
 SYORI-SUB                   SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG03           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "HEAD01"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**
** 処理区分入力
**
     MOVE  ZERO             TO  ERR-MSG-CD.
     EVALUATE  DSP-FUNC
       WHEN  "F005"
         MOVE  "END"        TO  END-FLG

       WHEN  "E000"
         PERFORM  SYORICHK-SUB
         IF ERR-MSG-CD = ZERO
            MOVE  "2"       TO  MAIN-FLG
            PERFORM  HEADDEL-SUB
         END-IF
       WHEN  OTHER
         MOVE 01            TO  ERR-MSG-CD
     END-EVALUATE.
*
 SYORI-END.
     EXIT.
*--------------------------------------------------------------*
*    画面表示処理                                              *
*--------------------------------------------------------------*
 DSP-WRITE-SUB               SECTION.
     MOVE  "SCREEN"         TO  DSP-GROUP.
     MOVE  SPACE            TO  DSP-PROC.
     MOVE  HEN-DATE         TO  DSP-SDATE.
     MOVE  HEN-TIME         TO  DSP-STIME.
     MOVE  "SSY386SI"       TO  DSP-PGID.
     MOVE  "FSY386S1"       TO  DSP-FORMID.
     WRITE  DSP-FSY386S1.
 DSP-WRITE-END.
     EXIT.
*--------------------------------------------------------------*
*    エラーメッセージセット                                    *
*--------------------------------------------------------------*
 MSG-SEC                     SECTION.
*    エラー メッセージ セット
     IF ERR-MSG-CD = ZERO
        MOVE  SPACE         TO  DSP-MSG1
     ELSE
        MOVE  ERR-MSG(ERR-MSG-CD)  TO  DSP-MSG1
        MOVE  ZERO                 TO  ERR-MSG-CD
     END-IF.

 MSG-END.
     EXIT.
*--------------------------------------------------------------*
*    画面データの入力処理                                      *
*--------------------------------------------------------------*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"             TO  DSP-PROC.
     READ  DSPF.

 DSP-READ-END.
     EXIT.
*--------------------------------------------------------------*
*    処理区分の入力チェック                                    *
*--------------------------------------------------------------*
 SYORICHK-SUB            SECTION.
*    処理区分 CHK
     IF ( DSP-SYORI  NOT  NUMERIC   )
        MOVE  11            TO  ERR-MSG-CD
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SYORI
        MOVE  "C"           TO  EDIT-CURSOR OF DSP-SYORI
     ELSE
        IF ( DSP-SYORI = 1 OR 2 OR 3 )
           IF DSP-SYORI = 1
              MOVE  1       TO  WK-SYORI
           END-IF
           IF DSP-SYORI = 2
              MOVE  2       TO  WK-SYORI
           END-IF
           IF DSP-SYORI = 3
              MOVE  3       TO  WK-SYORI
           END-IF
        ELSE
           MOVE  11         TO  ERR-MSG-CD
           MOVE  "R"        TO  EDIT-OPTION  OF  DSP-SYORI
           MOVE  "C"        TO  EDIT-CURSOR  OF  DSP-SYORI
        END-IF
     END-IF.
*
 SYORICHK-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部入力                                            *
*--------------------------------------------------------------*
 HEAD-SUB                    SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG01           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "HEAD02"         TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
** ヘッド部入力
     MOVE  ZERO             TO  ERR-MSG-CD.

*    アテンション判定
     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD

       WHEN  "E000"
         PERFORM  HEADCHK-SUB
         IF ERR-MSG-CD = ZERO
            MOVE  "D"       TO  EDIT-OPTION OF DSP-NFSYOC
            MOVE  SPACE     TO  EDIT-CURSOR OF DSP-NFSYOC
            IF WK-SYORI = 3
               MOVE  "5"    TO  MAIN-FLG
            ELSE
               MOVE  "3"    TO  MAIN-FLG
            END-IF
         END-IF
       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD
     END-EVALUATE.
*
 HEAD-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部消去                                            *
*--------------------------------------------------------------*
 HEADDEL-SUB            SECTION.
*
     MOVE  SPACE            TO  DSP-NFSYOC.
*
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SYORI.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYORI.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-NFSYOC.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-NFSYOC.
*
 HEADDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＨＥＡＤ部の入力チェック                                  *
*--------------------------------------------------------------*
 HEADCHK-SUB            SECTION.
*ナフコ商品コード入力チェック
     IF DSP-NFSYOC = SPACE
        MOVE  "R"    TO  EDIT-OPTION OF DSP-NFSYOC
        MOVE  04     TO  ERR-MSG-CD
        MOVE  "C"    TO  EDIT-CURSOR OF DSP-NFSYOC
        GO           TO  HEADCHK-END
     END-IF.
*ナフコ商品マスタ索引
     MOVE DSP-NFSYOC     TO  SHO-F01.
     READ NFSHOMS
       INVALID   KEY
         MOVE  "INV"        TO  NFSHOMS-INV-FLG
       NOT INVALID KEY
         MOVE  SPACE        TO  NFSHOMS-INV-FLG
     END-READ.
*判定
     IF NFSHOMS-INV-FLG = "INV"   *>ファイルに未存在の時
        IF WK-SYORI NOT = 1       *>登録以外の時、エラー
           MOVE  "R"        TO  EDIT-OPTION OF DSP-NFSYOC
           IF ERR-MSG-CD = ZERO
              MOVE  10      TO  ERR-MSG-CD
              MOVE  "C"     TO  EDIT-CURSOR OF DSP-NFSYOC
           END-IF
        END-IF
     ELSE
        IF WK-SYORI = 1 *>ファイルが存在し、登録の場合、エラー
           MOVE  "R"        TO  EDIT-OPTION OF DSP-NFSYOC
           IF ERR-MSG-CD = ZERO
              MOVE  02      TO  ERR-MSG-CD
              MOVE  "C"     TO  EDIT-CURSOR OF DSP-NFSYOC
           END-IF
        END-IF
     END-IF.
*エラーがあった場合は、エラーメッセージ出力
     IF ERR-MSG-CD NOT = ZERO
        GO TO  HEADCHK-END
     END-IF.
*
*##2019/12/05 INSERT START
********商品変換テーブル索引
     MOVE  284202           TO  TBL-F01.
     MOVE  DSP-NFSYOC       TO  TBL-F02.
     PERFORM HSHOTBL-READ-SEC.
     EVALUATE  WK-SYORI
********登録時、存在していたらエラー
       WHEN  1
           IF  HSHOTBL-INV-FLG = SPACE
               MOVE  "R"        TO  EDIT-OPTION OF DSP-NFSYOC
               IF ERR-MSG-CD = ZERO
                  MOVE  29      TO  ERR-MSG-CD
                  MOVE  "C"     TO  EDIT-CURSOR OF DSP-NFSYOC
               END-IF
           END-IF
********修正時、存在しなかったらエラー
       WHEN  2
           IF  HSHOTBL-INV-FLG = "INV"
               MOVE  "R"        TO  EDIT-OPTION OF DSP-NFSYOC
               IF ERR-MSG-CD = ZERO
                  MOVE  12      TO  ERR-MSG-CD
                  MOVE  "C"     TO  EDIT-CURSOR OF DSP-NFSYOC
               END-IF
           END-IF
********削除時、
       WHEN  3
           CONTINUE
     END-EVALUATE.
     IF ERR-MSG-CD NOT = ZERO
        GO TO  HEADCHK-END
     END-IF.
*##2019/12/05 END
*
*項目セット
     PERFORM  FILE-SUB.
*
 HEADCHK-END.
     EXIT.

*--------------------------------------------------------------*
*    ファイルセット                                            *
*--------------------------------------------------------------*
 FILE-SUB               SECTION.
     IF WK-SYORI = 1    *>登録の時
        MOVE  SPACE              TO  DSP-MEISAI
********ランク原価／売価初期化
        MOVE  ZERO               TO  DSP-GENTAN DSP-URITAN
        MOVE  ZERO               TO  DSP-GENKA1 DSP-BAIKA1
        MOVE  ZERO               TO  DSP-GENKA2 DSP-BAIKA2
        MOVE  ZERO               TO  DSP-GENKA3 DSP-BAIKA3
        MOVE  ZERO               TO  DSP-GENKA4 DSP-BAIKA4
        MOVE  ZERO               TO  DSP-GENKA5 DSP-BAIKA5
        MOVE  ZERO               TO  DSP-IRESU
********2015/10/26 NAV ST 箱単数には登録時、１をセット
        MOVE  1                  TO  DSP-IRESU
********2015/10/26 NAV ED 箱単数には登録時、１をセット
        MOVE  SPACE              TO  DSP-BUMON
*##2019/12/05 DELETE START
********登録時、マスタを索引して初期情報をセットする。
********商品変換テーブル索引（１３７６０７＋ナフコ商品ＣＤ）
*###### MOVE  284202             TO  TBL-F01
*###### MOVE  DSP-NFSYOC         TO  TBL-F02
*###### PERFORM HSHOTBL-READ-SEC
*###### IF  HSHOTBL-INV-FLG = "INV"
*######     MOVE  "R"        TO  EDIT-OPTION OF DSP-NFSYOC
*######     IF ERR-MSG-CD = ZERO
*######        MOVE  12      TO  ERR-MSG-CD
*######        MOVE  "C"     TO  EDIT-CURSOR OF DSP-NFSYOC
*######     END-IF
*###### ELSE
************商品変換テーブル情報をセット
*######     MOVE TBL-F031    TO  DSP-SHOCD   MEI-F011
*######     MOVE TBL-F0321   TO  DSP-TAN1    MEI-F0121
*######     MOVE TBL-F0322   TO  DSP-TAN2    MEI-F0122
*######     MOVE TBL-F0323   TO  DSP-TAN3    MEI-F0123
*######     MOVE TBL-F05     TO  DSP-GENTAN
*######     MOVE TBL-F06     TO  DSP-URITAN
************商品名取得の為、商品名称マスタ索引
*######     PERFORM HMEIMS-READ-SEC
*######     IF  HMEIMS-INV-FLG = "INV"
*######         MOVE  "R"    TO  EDIT-OPTION OF DSP-NFSYOC
*######         IF ERR-MSG-CD = ZERO
*######            MOVE  13  TO  ERR-MSG-CD
*######            MOVE  "C" TO  EDIT-CURSOR OF DSP-NFSYOC
*######         END-IF
*######     ELSE
*######         MOVE MEI-F021 TO DSP-SHON1
*######         MOVE MEI-F022 TO DSP-SHON2
*######         MOVE MEI-F031 TO DSP-KANA1
*######         MOVE MEI-F032 TO DSP-KANA2
*######     END-IF
*###### END-IF
*##2019/12/05 END
*##2019/12/05 INSERT START
        MOVE  ZERO               TO  DSP-STAN
        MOVE  PARA-TOKCDI(3:6)   TO  DSP-TOKC
*##2019/12/05 END
     ELSE               *>修正／削除の時
        MOVE  SPACE              TO  DSP-MEISAI *>初期化
        MOVE  SHO-F02            TO  DSP-SHOCD  *>サカタ商品ＣＤ
        MOVE  SHO-F03(1:5)       TO  DSP-TAN1   *>品単１
        MOVE  SHO-F03(6:2)       TO  DSP-TAN2   *>品単２
        MOVE  SHO-F03(8:1)       TO  DSP-TAN3   *>品単３
        MOVE  SHO-F04            TO  DSP-JANCD  *>ＪＡＮＣＤ
        MOVE  SHO-F05            TO  DSP-SHON1  *>商品名漢字
        MOVE  SHO-F06            TO  DSP-SHON2  *>規格名漢字
        MOVE  SHO-F07            TO  DSP-KANA1  *>商品名カナ
        MOVE  SHO-F08            TO  DSP-KANA2  *>規格名カナ
        MOVE  SHO-F09            TO  DSP-IRISU  *>入数
        MOVE  SHO-F10            TO  DSP-IRESU  *>１箱当りケース
        MOVE  SHO-F11            TO  DSP-GENTAN *>標準原価単価
        MOVE  SHO-F12            TO  DSP-URITAN *>標準売価単価
        MOVE  SHO-F13            TO  DSP-GENKA1 *>ランク原単価１
        MOVE  SHO-F14            TO  DSP-BAIKA1 *>ランク売単価１
        MOVE  SHO-F15            TO  DSP-GENKA2 *>ランク原単価２
        MOVE  SHO-F16            TO  DSP-BAIKA2 *>ランク売単価２
        MOVE  SHO-F17            TO  DSP-GENKA3 *>ランク原単価３
        MOVE  SHO-F18            TO  DSP-BAIKA3 *>ランク売単価３
        MOVE  SHO-F19            TO  DSP-GENKA4 *>ランク原単価４
        MOVE  SHO-F20            TO  DSP-BAIKA4 *>ランク売単価４
        MOVE  SHO-F21            TO  DSP-GENKA5 *>ランク原単価５
        MOVE  SHO-F22            TO  DSP-BAIKA5 *>ランク売単価５
        MOVE  SHO-F23            TO  DSP-SAKUBC *>標準作場ＣＤ
        MOVE  SHO-F23            TO  SAK-F01    *>標準作場名
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUBN
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUBN
        END-IF
        MOVE  SHO-F44            TO  DSP-BUMON  *>部門
*##2019/12/05 INSERT START
        IF  HSHOTBL-INV-FLG = SPACE
            MOVE TBL-F081     TO  DSP-TANA(1:3) *>棚番(1:3)
            MOVE TBL-F082     TO  DSP-TANA(4:1) *>棚番(4:1)
            MOVE TBL-F083     TO  DSP-TANA(5:2) *>棚番(5:2)
            MOVE TBL-F04      TO  DSP-SYKA      *>出荷場所
            MOVE TBL-F09      TO  DSP-STAN      *>仕入単価
            MOVE TBL-F01      TO  DSP-TOKC      *>取引先コード
        END-IF
*##2019/12/05 END
*##2016/08/19 NAV ST 商品分類ＣＤの追加
        MOVE  SHO-F45            TO  DSP-SYOBUN *>商品分類ＣＤ
*##2016/08/19 NAV ED
      IF  SHO-F24 NOT = SPACE
        MOVE  SHO-F24            TO  DSP-AREA1  *>エリア１
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F24            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN1
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN1
        END-IF
      END-IF
      IF  SHO-F25 NOT = SPACE
        MOVE  SHO-F25           TO   DSP-SAKUB1 *>エリア作場１
        MOVE  SHO-F25           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN1
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN1
        END-IF
      END-IF
      IF  SHO-F26 NOT = SPACE
        MOVE  SHO-F26            TO  DSP-AREA2  *>エリア２
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F26            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN2
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN2
        END-IF
      END-IF
      IF  SHO-F27 NOT = SPACE
        MOVE  SHO-F27           TO   DSP-SAKUB2 *>エリア作場２
        MOVE  SHO-F27           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN2
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN2
        END-IF
      END-IF
      IF  SHO-F28 NOT = SPACE
        MOVE  SHO-F28            TO  DSP-AREA3  *>エリア３
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F28            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN3
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN3
        END-IF
      END-IF
      IF  SHO-F29 NOT = SPACE
        MOVE  SHO-F29           TO   DSP-SAKUB3 *>エリア作場３
        MOVE  SHO-F29           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN3
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN3
        END-IF
      END-IF
      IF  SHO-F30 NOT = SPACE
        MOVE  SHO-F30            TO  DSP-AREA4  *>エリア4
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F30            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN4
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN4
        END-IF
      END-IF
      IF  SHO-F31 NOT = SPACE
        MOVE  SHO-F31           TO   DSP-SAKUB4 *>エリア作場4
        MOVE  SHO-F31           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN4
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN4
        END-IF
      END-IF
      IF  SHO-F32 NOT = SPACE
        MOVE  SHO-F32            TO  DSP-AREA5  *>エリア5
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F32            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN5
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN5
        END-IF
      END-IF
      IF  SHO-F33 NOT = SPACE
        MOVE  SHO-F33           TO   DSP-SAKUB5 *>エリア作場5
        MOVE  SHO-F33           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN5
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN5
        END-IF
      END-IF
      IF  SHO-F34 NOT = SPACE
        MOVE  SHO-F34            TO  DSP-AREA6  *>エリア6
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F34            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN6
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN6
        END-IF
      END-IF
      IF  SHO-F35 NOT = SPACE
        MOVE  SHO-F35           TO   DSP-SAKUB6 *>エリア作場6
        MOVE  SHO-F35           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN6
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN6
        END-IF
      END-IF
      IF  SHO-F36 NOT = SPACE
        MOVE  SHO-F36            TO  DSP-AREA7  *>エリア7
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F36            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN7
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN7
        END-IF
      END-IF
      IF  SHO-F37 NOT = SPACE
        MOVE  SHO-F37           TO   DSP-SAKUB7 *>エリア作場7
        MOVE  SHO-F37           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN7
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN7
        END-IF
      END-IF
      IF  SHO-F38 NOT = SPACE
        MOVE  SHO-F38            TO  DSP-AREA8  *>エリア8
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F38            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN8
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN8
        END-IF
      END-IF
      IF  SHO-F39 NOT = SPACE
        MOVE  SHO-F39           TO   DSP-SAKUB8 *>エリア作場8
        MOVE  SHO-F39           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN8
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN8
        END-IF
      END-IF
      IF  SHO-F40 NOT = SPACE
        MOVE  SHO-F40            TO  DSP-AREA9  *>エリア9
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F40            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERN9
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERN9
        END-IF
      END-IF
      IF  SHO-F41 NOT = SPACE
        MOVE  SHO-F41           TO   DSP-SAKUB9 *>エリア作場9
        MOVE  SHO-F41           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUN9
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUN9
        END-IF
      END-IF
      IF  SHO-F42 NOT = SPACE
        MOVE  SHO-F42            TO  DSP-AREA10 *>エリア10
        MOVE  "96"               TO  JYO-F01
        MOVE  SHO-F42            TO  JYO-F02
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-ARERNA
        ELSE
            MOVE  JYO-F03       TO   DSP-ARERNA
        END-IF
      END-IF
      IF  SHO-F43 NOT = SPACE
        MOVE  SHO-F43           TO   DSP-SAKUBA *>エリア作場10
        MOVE  SHO-F43           TO   SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  ALL NC"＊"    TO   DSP-SAKUNA
        ELSE
            MOVE     SAK-F02    TO   DSP-SAKUNA
        END-IF
      END-IF
     END-IF.
*
 FILE-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ部入力                                            *
*--------------------------------------------------------------*
 BODY-SUB          SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG02           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "BODY"           TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**  ボディー部入力
     MOVE  ZERO             TO  ERR-MSG-CD.

     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "2"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
       WHEN  "F006"
         MOVE   "2"         TO  MAIN-FLG
         MOVE   ZERO        TO  ERR-MSG-CD
       WHEN  "E000"
         PERFORM  BODYCHK-SUB
         IF ERR-MSG-CD = ZERO
            PERFORM  BODYDEL-SUB
            MOVE  "4"       TO  MAIN-FLG
         END-IF
       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD
     END-EVALUATE.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ部入力（エリア情報）                              *
*--------------------------------------------------------------*
 BODY2-SUB         SECTION.
     PERFORM  MSG-SEC.
     MOVE  PMSG02           TO  DSP-MSG2.
     PERFORM  DSP-WRITE-SUB.
     MOVE  "BODY2"          TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
**  ボディー部入力
     MOVE  ZERO             TO  ERR-MSG-CD.

     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "2"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
       WHEN  "F006"
         MOVE   "3"         TO  MAIN-FLG
         MOVE   ZERO        TO  ERR-MSG-CD
       WHEN  "E000"
         PERFORM  BODYCHK2-SUB
         IF ERR-MSG-CD = ZERO
            PERFORM  BODYDEL-SUB
            MOVE  "5"       TO  MAIN-FLG
         END-IF
       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD
     END-EVALUATE.
*
 BODY-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ部消去                                            *
*--------------------------------------------------------------*
 BODYDEL-SUB            SECTION.
*
*****MOVE  SPACE            TO  DSP-MEISAI.
*
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SHOCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHOCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TAN3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TAN3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-JANCD.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-JANCD.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SHON1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHON1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SHON2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SHON2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KANA1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KANA1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-KANA2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-KANA2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-IRISU.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-IRISU.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-IRESU.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-IRESU.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENTAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENTAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-URITAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-URITAN.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BUMON.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BUMON.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUBC.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUBC.
*##2019/12/05 INSERT START
     MOVE  "D"              TO  EDIT-OPTION OF DSP-TANA.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-TANA.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SYKA.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYKA.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-STAN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-STAN.
*##2019/12/05 END
*##2016/08/19 NAV ST
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SYOBUN.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SYOBUN.
*##2016/08/19 NAV ED
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA1
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA2
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA3
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA4.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA4
     MOVE  "D"              TO  EDIT-OPTION OF DSP-GENKA5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-GENKA5.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BAIKA5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BAIKA5.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-BUMON.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-BUMON.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB1.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB1.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB2.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB2.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB3.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB3.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA4.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB4.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB4.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA5.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB5.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB5.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA6.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA6.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB6.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB6.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA7.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA7.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB7.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB7.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA8.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA8.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB8.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB8.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA9.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA9.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUB9.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUB9.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-AREA10.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-AREA10.
     MOVE  "D"              TO  EDIT-OPTION OF DSP-SAKUBA.
     MOVE  SPACE            TO  EDIT-CURSOR OF DSP-SAKUBA.
 BODYDEL-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ入力チェック                                      *
*--------------------------------------------------------------*
 BODYCHK-SUB            SECTION.
*
     MOVE  ZERO             TO  ERR-MSG-CD.
     PERFORM  BODYDEL-SUB.
*サカタ商品ＣＤ入力チェック
     IF DSP-SHOCD = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SHOCD
        IF ERR-MSG-CD = ZERO
           MOVE  03         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SHOCD
        END-IF
     ELSE
********商品マスタ存在チェック
********商品変換テーブル情報をセット
            MOVE DSP-SHOCD   TO  MEI-F011
            MOVE DSP-TAN1    TO  MEI-F0121
            MOVE DSP-TAN2    TO  MEI-F0122
            MOVE DSP-TAN3    TO  MEI-F0123
************商品名取得の為、商品名称マスタ索引
            PERFORM HMEIMS-READ-SEC
            IF  HMEIMS-INV-FLG = "INV"
*T
*               DISPLAY "DSP-SHOCD=" DSP-SHOCD UPON CONS
*               DISPLAY "DSP-TAN1 =" DSP-TAN1  UPON CONS
*               DISPLAY "DSP-TAN2 =" DSP-TAN2  UPON CONS
*               DISPLAY "DSP-TAN3 =" DSP-TAN3  UPON CONS
*               DISPLAY "MEI-F011 =" MEI-F011  UPON CONS
*               DISPLAY "MEI-F0121=" MEI-F0121 UPON CONS
*               DISPLAY "MEI-F0122=" MEI-F0122 UPON CONS
*               DISPLAY "MEI-F0123=" MEI-F0123 UPON CONS
*T
                MOVE  "R"    TO  EDIT-OPTION OF DSP-SHOCD
                MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN1
                MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN2
                MOVE  "R"    TO  EDIT-OPTION OF DSP-TAN3
                IF ERR-MSG-CD = ZERO
                   MOVE  04  TO  ERR-MSG-CD
                   MOVE  "C" TO  EDIT-CURSOR OF DSP-SHOCD
                END-IF
            END-IF
     END-IF.
*ＪＡＮＣＤ未入力チェック
     IF DSP-JANCD = SPACE
     OR DSP-JANCD(1:1) = SPACE
     OR DSP-JANCD(2:1) = SPACE
     OR DSP-JANCD(3:1) = SPACE
     OR DSP-JANCD(4:1) = SPACE
     OR DSP-JANCD(5:1) = SPACE
     OR DSP-JANCD(6:1) = SPACE
     OR DSP-JANCD(7:1) = SPACE
     OR DSP-JANCD(8:1) = SPACE
     OR DSP-JANCD(9:1) = SPACE
     OR DSP-JANCD(10:1) = SPACE
     OR DSP-JANCD(11:1) = SPACE
     OR DSP-JANCD(12:1) = SPACE
     OR DSP-JANCD(13:1) = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-JANCD
        IF ERR-MSG-CD = ZERO
           MOVE  05         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-JANCD
        END-IF
     END-IF.
*商品名漢字入力チェック
     IF DSP-SHON1 = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SHON1
        IF ERR-MSG-CD = ZERO
           MOVE  14         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SHON1
        END-IF
     END-IF.
*商品名カナ入力チェック
     IF DSP-KANA1 = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-KANA1
        IF ERR-MSG-CD = ZERO
           MOVE  15         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-KANA1
        END-IF
     END-IF.
*入数は特にチェックなし
*標準原価単価
     IF DSP-GENTAN NOT NUMERIC
     OR DSP-GENTAN  = ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-GENTAN
        IF ERR-MSG-CD = ZERO
           MOVE  06         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENTAN
        END-IF
     END-IF.
*標準売価単価
     IF DSP-URITAN NOT NUMERIC
     OR DSP-URITAN  =  ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-URITAN
        IF ERR-MSG-CD = ZERO
           MOVE  07         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-URITAN
        END-IF
     END-IF.
*2015/10/26 NAV ST 箱単数チェック追加
*標準原価単価
     IF DSP-IRESU NOT NUMERIC
     OR DSP-IRESU  = ZERO
        MOVE  "R"           TO  EDIT-OPTION OF DSP-IRESU
        IF ERR-MSG-CD = ZERO
           MOVE  24         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-IRESU
        END-IF
     END-IF.
*2015/10/26 NAV ED 箱単数チェック追加
*原価／売価大小チェック
     IF  DSP-GENTAN  NUMERIC
     AND DSP-URITAN  NUMERIC
        IF  DSP-GENTAN > DSP-URITAN
            MOVE  "R"           TO  EDIT-OPTION OF DSP-GENTAN
            MOVE  "R"           TO  EDIT-OPTION OF DSP-URITAN
            IF ERR-MSG-CD = ZERO
               MOVE  08         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENTAN
            END-IF
        END-IF
     END-IF.
*ランク原価／売価大小チェック１
     IF  DSP-GENKA1 > DSP-BAIKA1
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA1
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA1
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA1
         END-IF
     END-IF.
*ランク原価／売価大小チェック２
     IF  DSP-GENKA2 > DSP-BAIKA2
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA2
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA2
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA2
         END-IF
     END-IF.
*ランク原価／売価大小チェック３
     IF  DSP-GENKA3 > DSP-BAIKA3
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA3
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA3
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA3
         END-IF
     END-IF.
*ランク原価／売価大小チェック４
     IF  DSP-GENKA4 > DSP-BAIKA4
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA4
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA4
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA4
         END-IF
     END-IF.
*ランク原価／売価大小チェック５
     IF  DSP-GENKA5 > DSP-BAIKA5
         MOVE  "R"           TO  EDIT-OPTION OF DSP-GENKA5
         MOVE  "R"           TO  EDIT-OPTION OF DSP-BAIKA5
         IF ERR-MSG-CD = ZERO
            MOVE  09         TO  ERR-MSG-CD
            MOVE  "C"        TO  EDIT-CURSOR OF DSP-GENKA5
         END-IF
     END-IF.
*部門チェック
     IF DSP-BUMON = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-BUMON
        IF ERR-MSG-CD = ZERO
           MOVE  16         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-BUMON
        END-IF
     END-IF.
*標準作場チェック
     IF DSP-SAKUBC = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUBC
        IF ERR-MSG-CD = ZERO
           MOVE  17         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUBC
        END-IF
     ELSE
********作場マスタ存在チェック
            MOVE DSP-SAKUBC  TO  SAK-F01
************作場マスタ存在チェック
            PERFORM SAKUBAF-READ-SEC
            IF  SAKUBAF-INV-FLG = "INV"
                MOVE  "R"    TO  EDIT-OPTION OF DSP-SAKUBC
                IF ERR-MSG-CD = ZERO
                   MOVE  18  TO  ERR-MSG-CD
                   MOVE  "C" TO  EDIT-CURSOR OF DSP-SAKUBC
                END-IF
            ELSE
                MOVE SAK-F02 TO  DSP-SAKUBN
            END-IF
     END-IF.
*##2019/12/05 INSERT START
*棚番チェック　（なし）
*出荷場所チェック
     IF DSP-SYKA   = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SYKA
        IF ERR-MSG-CD = ZERO
           MOVE  27         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SYKA
        END-IF
     ELSE
********倉庫マスタ存在チェック
            MOVE DSP-SYKA    TO  SOK-F01
************倉庫マスタ存在チェック
            PERFORM ZSOKMS-READ-SEC
            IF  ZSOKMS-INV-FLG = "INV"
                MOVE  "R"    TO  EDIT-OPTION OF DSP-SYKA
                IF ERR-MSG-CD = ZERO
                   MOVE  28  TO  ERR-MSG-CD
                   MOVE  "C" TO  EDIT-CURSOR OF DSP-SYKA
                END-IF
            END-IF
     END-IF.
*仕入単価チェック　（なし）
*##2019/12/05 END
*
*##2016/08/19 NAV ST 商品分類ＣＤの追加
*標準作場チェック
     IF DSP-SYOBUN  = SPACE
        MOVE  "R"           TO  EDIT-OPTION OF DSP-SYOBUN
        IF ERR-MSG-CD = ZERO
           MOVE  25         TO  ERR-MSG-CD
           MOVE  "C"        TO  EDIT-CURSOR OF DSP-SYOBUN
        END-IF
     ELSE
********条件Ｆ存在チェック
            MOVE 05          TO  JYO-F01
            MOVE DSP-SYOBUN  TO  JYO-F02
************条件Ｆ存在チェック
            PERFORM HJYOKEN-READ-SEC
            IF  HJYOKEN-INV-FLG = "INV"
                MOVE  "R"    TO  EDIT-OPTION OF DSP-SYOBUN
                IF ERR-MSG-CD = ZERO
                   MOVE  26  TO  ERR-MSG-CD
                   MOVE  "C" TO  EDIT-CURSOR OF DSP-SYOBUN
                END-IF
            END-IF
     END-IF.
*##2016/08/19 NAV ED
*
 BODYCHK-END.
     EXIT.
*--------------------------------------------------------------*
*    ＢＯＤＹ入力チェック                                      *
*--------------------------------------------------------------*
 BODYCHK2-SUB           SECTION.
*
     MOVE  ZERO             TO  ERR-MSG-CD.
*属性初期化
     PERFORM  BODYDEL-SUB.
     MOVE     SPACE             TO  DSP-ARERN1.
     MOVE     SPACE             TO  DSP-ARERN2.
     MOVE     SPACE             TO  DSP-ARERN3.
     MOVE     SPACE             TO  DSP-ARERN4.
     MOVE     SPACE             TO  DSP-ARERN5.
     MOVE     SPACE             TO  DSP-ARERN6.
     MOVE     SPACE             TO  DSP-ARERN7.
     MOVE     SPACE             TO  DSP-ARERN8.
     MOVE     SPACE             TO  DSP-ARERN9.
     MOVE     SPACE             TO  DSP-ARERNA.
     MOVE     SPACE             TO  DSP-SAKUN1.
     MOVE     SPACE             TO  DSP-SAKUN2.
     MOVE     SPACE             TO  DSP-SAKUN3.
     MOVE     SPACE             TO  DSP-SAKUN4.
     MOVE     SPACE             TO  DSP-SAKUN5.
     MOVE     SPACE             TO  DSP-SAKUN6.
     MOVE     SPACE             TO  DSP-SAKUN7.
     MOVE     SPACE             TO  DSP-SAKUN8.
     MOVE     SPACE             TO  DSP-SAKUN9.
     MOVE     SPACE             TO  DSP-SAKUNA.
     MOVE     SPACE             TO  AREA-TBL-CHK.
*エリア１チェック
     IF DSP-AREA1 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA1     TO  JYO-F02  AREA-TBLCD(01)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA1
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA1
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN1
        END-IF
     END-IF.
*作場１チェック
     IF DSP-SAKUB1 NOT = SPACE
        MOVE  DSP-SAKUB1    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB1
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB1
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN1
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA1  = SPACE ) AND ( DSP-SAKUB1 NOT = SPACE ))
     OR (( DSP-AREA1  NOT = SPACE ) AND ( DSP-SAKUB1 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA1
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB1
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA1
           END-IF
     END-IF.
*エリア２チェック
     IF DSP-AREA2 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA2     TO  JYO-F02  AREA-TBLCD(02)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA2
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA2
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN2
        END-IF
     END-IF.
*作場２チェック
     IF DSP-SAKUB2 NOT = SPACE
        MOVE  DSP-SAKUB2    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB2
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB2
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN2
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA2  = SPACE ) AND ( DSP-SAKUB2 NOT = SPACE ))
     OR (( DSP-AREA2  NOT = SPACE ) AND ( DSP-SAKUB2 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA2
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB2
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA2
           END-IF
     END-IF.
*エリア３チェック
     IF DSP-AREA3 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA3     TO  JYO-F02  AREA-TBLCD(03)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA3
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA3
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN3
        END-IF
     END-IF.
*作場３チェック
     IF DSP-SAKUB3 NOT = SPACE
        MOVE  DSP-SAKUB3    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB3
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB3
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN3
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA3  = SPACE ) AND ( DSP-SAKUB3 NOT = SPACE ))
     OR (( DSP-AREA3  NOT = SPACE ) AND ( DSP-SAKUB3 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA3
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB3
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA3
           END-IF
     END-IF.
*エリア４チェック
     IF DSP-AREA4 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA4     TO  JYO-F02  AREA-TBLCD(04)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA4
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA4
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN4
        END-IF
     END-IF.
*作場４チェック
     IF DSP-SAKUB4 NOT = SPACE
        MOVE  DSP-SAKUB4    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB4
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB4
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN4
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA4  = SPACE ) AND ( DSP-SAKUB4 NOT = SPACE ))
     OR (( DSP-AREA4  NOT = SPACE ) AND ( DSP-SAKUB4 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA4
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB4
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA4
           END-IF
     END-IF.
*エリア５チェック
     IF DSP-AREA5 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA5     TO  JYO-F02  AREA-TBLCD(05)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA5
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA5
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN5
        END-IF
     END-IF.
*作場５チェック
     IF DSP-SAKUB5 NOT = SPACE
        MOVE  DSP-SAKUB5    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB5
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB5
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN5
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA5  = SPACE ) AND ( DSP-SAKUB5 NOT = SPACE ))
     OR (( DSP-AREA5  NOT = SPACE ) AND ( DSP-SAKUB5 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA5
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB5
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA5
           END-IF
     END-IF.
*エリア６チェック
     IF DSP-AREA6 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA6     TO  JYO-F02  AREA-TBLCD(06)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA6
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA6
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN6
        END-IF
     END-IF.
*作場６チェック
     IF DSP-SAKUB6 NOT = SPACE
        MOVE  DSP-SAKUB6    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB6
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB6
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN6
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA6  = SPACE ) AND ( DSP-SAKUB6 NOT = SPACE ))
     OR (( DSP-AREA6  NOT = SPACE ) AND ( DSP-SAKUB6 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA6
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB6
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA6
           END-IF
     END-IF.
*エリア７チェック
     IF DSP-AREA7 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA7     TO  JYO-F02  AREA-TBLCD(07)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA7
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA7
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN7
        END-IF
     END-IF.
*作場７チェック
     IF DSP-SAKUB7 NOT = SPACE
        MOVE  DSP-SAKUB7    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB7
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB7
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN7
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA7  = SPACE ) AND ( DSP-SAKUB7 NOT = SPACE ))
     OR (( DSP-AREA7  NOT = SPACE ) AND ( DSP-SAKUB7 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA7
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB7
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA7
           END-IF
     END-IF.
*エリア８チェック
     IF DSP-AREA8 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA8     TO  JYO-F02  AREA-TBLCD(08)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA8
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA8
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN8
        END-IF
     END-IF.
*作場８チェック
     IF DSP-SAKUB8 NOT = SPACE
        MOVE  DSP-SAKUB8    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB8
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB8
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN8
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA8  = SPACE ) AND ( DSP-SAKUB8 NOT = SPACE ))
     OR (( DSP-AREA8  NOT = SPACE ) AND ( DSP-SAKUB8 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA8
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB8
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA8
           END-IF
     END-IF.
*エリア９チェック
     IF DSP-AREA9 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA9     TO  JYO-F02  AREA-TBLCD(09)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA9
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA9
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERN9
        END-IF
     END-IF.
*作場９チェック
     IF DSP-SAKUB9 NOT = SPACE
        MOVE  DSP-SAKUB9    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB9
            IF ERR-MSG-CD = ZERO
               MOVE  18         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUB9
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUN9
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA9  = SPACE ) AND ( DSP-SAKUB9 NOT = SPACE ))
     OR (( DSP-AREA9  NOT = SPACE ) AND ( DSP-SAKUB9 = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA9
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUB9
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA9
           END-IF
     END-IF.
*エリアＡチェック
     IF DSP-AREA10 NOT = SPACE
        MOVE  "96"          TO  JYO-F01
        MOVE  DSP-AREA10    TO  JYO-F02  AREA-TBLCD(10)
        PERFORM HJYOKEN-READ-SEC
        IF  HJYOKEN-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA10
            IF ERR-MSG-CD = ZERO
               MOVE  19         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA10
            END-IF
        ELSE
            MOVE     JYO-F03    TO  DSP-ARERNA
        END-IF
     END-IF.
*作場Ａチェック
     IF DSP-SAKUBA NOT = SPACE
        MOVE  DSP-SAKUBA    TO  SAK-F01
        PERFORM SAKUBAF-READ-SEC
        IF  SAKUBAF-INV-FLG = "INV"
            MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUBA
            IF ERR-MSG-CD = ZERO
               MOVE  20         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-SAKUBA
            END-IF
        ELSE
            MOVE     SAK-F02    TO  DSP-SAKUNA
        END-IF
     END-IF.
*    どちらか一方の場合
     IF (( DSP-AREA10 = SPACE ) AND ( DSP-SAKUBA NOT = SPACE ))
     OR (( DSP-AREA10 NOT = SPACE ) AND ( DSP-SAKUBA = SPACE ))
           MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA10
           MOVE  "R"           TO  EDIT-OPTION OF DSP-SAKUBA
           IF ERR-MSG-CD = ZERO
              MOVE  21         TO  ERR-MSG-CD
              MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA10
           END-IF
     END-IF.
*ＴＢＬ内エリアＣＤ重複チェック
     MOVE  1                   TO  IY.
     MOVE  DSP-AREA1           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA1
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA1
            END-IF
     END-IF.
     MOVE  2                   TO  IY.
     MOVE  DSP-AREA2           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA2
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA2
            END-IF
     END-IF.
     MOVE  3                   TO  IY.
     MOVE  DSP-AREA3           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA3
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA3
            END-IF
     END-IF.
     MOVE  4                   TO  IY.
     MOVE  DSP-AREA4           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA4
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA4
            END-IF
     END-IF.
     MOVE  5                   TO  IY.
     MOVE  DSP-AREA5           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA5
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA5
            END-IF
     END-IF.
     MOVE  6                   TO  IY.
     MOVE  DSP-AREA6           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA6
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA6
            END-IF
     END-IF.
     MOVE  7                   TO  IY.
     MOVE  DSP-AREA7           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA7
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA7
            END-IF
     END-IF.
     MOVE  8                   TO  IY.
     MOVE  DSP-AREA8           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA8
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA8
            END-IF
     END-IF.
     MOVE  9                   TO  IY.
     MOVE  DSP-AREA9           TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA9
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA9
            END-IF
     END-IF.
     MOVE  10                  TO  IY.
     MOVE  DSP-AREA10          TO  AREA-TBL-WK.
     PERFORM  AREASAKUBA-CHK-SUB.
     IF    ERR-KBN NOT = SPACE
            MOVE  "R"           TO  EDIT-OPTION OF DSP-AREA10
            IF ERR-MSG-CD = ZERO
               MOVE  22         TO  ERR-MSG-CD
               MOVE  "C"        TO  EDIT-CURSOR OF DSP-AREA10
            END-IF
     END-IF.
*
 BODYCHK2-END.
     EXIT.
*--------------------------------------------------------------*
*    エリア＋作場テーブルチェック                              *
*--------------------------------------------------------------*
 AREASAKUBA-CHK-SUB    SECTION.
*エリア作場ＴＢＬ重複チェック
     MOVE     SPACE                 TO     ERR-KBN.
     PERFORM  VARYING  IX  FROM  1  BY  1  UNTIL  IX > 10
              IF  IX  NOT =  IY  AND AREA-TBLCD(IX) NOT = SPACE
                  IF  AREA-TBL-WK  =  AREA-TBLCD(IX)
                      MOVE "ERR"    TO     ERR-KBN
                  END-IF
              END-IF
     END-PERFORM.
*
 AREASAKUBA-END.
     EXIT.
*--------------------------------------------------------------*
*    確認入力                                                  *
*--------------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     IF ERR-MSG-CD = ZERO
        MOVE  "Y"           TO  DSP-KAKUNI
     END-IF.

     PERFORM  MSG-SEC.
     IF WK-SYORI = 3
        MOVE  PMSG01        TO  DSP-MSG2
     ELSE
        MOVE  PMSG02        TO  DSP-MSG2
     END-IF.

     PERFORM  DSP-WRITE-SUB.
     MOVE  "KAKU"           TO  DSP-GROUP.
     PERFORM  DSP-READ-SUB.
     MOVE  ZERO             TO  ERR-MSG-CD.
 KAKUNIN.
** 確認
     EVALUATE  DSP-FUNC
       WHEN  "F004"
         PERFORM  HEADDEL-SUB
         MOVE  SPACE            TO  DSP-MEISAI
         PERFORM  BODYDEL-SUB
         MOVE  "1"          TO  MAIN-FLG
         MOVE  ZERO         TO  ERR-MSG-CD
         MOVE  SPACE        TO  DSP-KAKUNI
       WHEN  "F006"
         IF WK-SYORI = 3
            MOVE  01        TO  ERR-MSG-CD
         ELSE
            MOVE  "4"       TO  MAIN-FLG
         END-IF
         MOVE  SPACE        TO  DSP-KAKUNI
       WHEN  "E000"
         IF DSP-KAKUNI NOT = "Y"
            MOVE  23        TO  ERR-MSG-CD
         ELSE
            MOVE  SPACE     TO  DSP-KAKUNI
            MOVE  "6"       TO  MAIN-FLG
         END-IF
       WHEN  OTHER
         MOVE  01           TO  ERR-MSG-CD

     END-EVALUATE.
*
 KAKUNIN-END.
     EXIT.
*--------------------------------------------------------------*
*    ファイル更新                                              *
*--------------------------------------------------------------*
 FILPRT-SUB             SECTION.
     IF WK-SYORI =  1
        MOVE  SPACE         TO  SHO-REC
        INITIALIZE              SHO-REC
     END-IF.
*
*##2019/12/05 INSERT START
     MOVE  PARA-TOKCDI      TO  SHO-F00.
*##2019/12/05 END
     MOVE  DSP-NFSYOC       TO  SHO-F01.
     MOVE  DSP-SHOCD        TO  SHO-F02.
     MOVE  DSP-TAN1         TO  SHO-F03(1:5).
     MOVE  DSP-TAN2         TO  SHO-F03(6:2).
     MOVE  DSP-TAN3         TO  SHO-F03(8:1).
     MOVE  DSP-JANCD        TO  SHO-F04.
     MOVE  DSP-SHON1        TO  SHO-F05.
     MOVE  DSP-SHON2        TO  SHO-F06.
     MOVE  DSP-KANA1        TO  SHO-F07.
     MOVE  DSP-KANA2        TO  SHO-F08.
     MOVE  DSP-IRISU        TO  SHO-F09.
     MOVE  DSP-IRESU        TO  SHO-F10.
     MOVE  DSP-GENTAN       TO  SHO-F11.
     MOVE  DSP-URITAN       TO  SHO-F12.
     MOVE  DSP-GENKA1       TO  SHO-F13.
     MOVE  DSP-BAIKA1       TO  SHO-F14.
     MOVE  DSP-GENKA2       TO  SHO-F15.
     MOVE  DSP-BAIKA2       TO  SHO-F16.
     MOVE  DSP-GENKA3       TO  SHO-F17.
     MOVE  DSP-BAIKA3       TO  SHO-F18.
     MOVE  DSP-GENKA4       TO  SHO-F19.
     MOVE  DSP-BAIKA4       TO  SHO-F20.
     MOVE  DSP-GENKA5       TO  SHO-F21.
     MOVE  DSP-BAIKA5       TO  SHO-F22.
*標準作場ＣＤ
     MOVE  DSP-SAKUBC       TO  SHO-F23.
*部門
     MOVE  DSP-BUMON        TO  SHO-F44.
*##2016/08/19 NAV ST
     MOVE  DSP-SYOBUN       TO  SHO-F45.
*##2016/08/19 NAV ED
*エリア＋エリア作場更新
     MOVE  DSP-AREA1        TO  SHO-F24.
     MOVE  DSP-SAKUB1       TO  SHO-F25.
     MOVE  DSP-AREA2        TO  SHO-F26.
     MOVE  DSP-SAKUB2       TO  SHO-F27.
     MOVE  DSP-AREA3        TO  SHO-F28.
     MOVE  DSP-SAKUB3       TO  SHO-F29.
     MOVE  DSP-AREA4        TO  SHO-F30.
     MOVE  DSP-SAKUB4       TO  SHO-F31.
     MOVE  DSP-AREA5        TO  SHO-F32.
     MOVE  DSP-SAKUB5       TO  SHO-F33.
     MOVE  DSP-AREA6        TO  SHO-F34.
     MOVE  DSP-SAKUB6       TO  SHO-F35.
     MOVE  DSP-AREA7        TO  SHO-F36.
     MOVE  DSP-SAKUB7       TO  SHO-F37.
     MOVE  DSP-AREA8        TO  SHO-F38.
     MOVE  DSP-SAKUB8       TO  SHO-F39.
     MOVE  DSP-AREA9        TO  SHO-F40.
     MOVE  DSP-SAKUB9       TO  SHO-F41.
     MOVE  DSP-AREA10       TO  SHO-F42.
     MOVE  DSP-SAKUBA       TO  SHO-F43.
*担当者情報更新
     MOVE  PARA-BUMONCDI    TO  SHO-F97.
     MOVE  PARA-TANCDI(1:2) TO  SHO-F98.
     MOVE  SYS-DATE         TO  SHO-F99.
*    処理モードにより追加・更新・削除
     EVALUATE  WK-SYORI
       WHEN  1
         MOVE  PARA-BUMONCDI    TO  SHO-F94
         MOVE  PARA-TANCDI(1:2) TO  SHO-F95
         MOVE  SYS-DATE         TO  SHO-F96
         WRITE  SHO-REC
       WHEN  2
         REWRITE  SHO-REC
       WHEN  3
         DELETE  NFSHOMS
     END-EVALUATE.
*
*##2019/12/05 INSERT START
*商品変換テーブル更新処理
     IF WK-SYORI =  1
        MOVE  SPACE         TO  TBL-REC
        INITIALIZE              TBL-REC
        MOVE  PARA-TOKCDI   TO  TBL-F01
        MOVE  DSP-NFSYOC    TO  TBL-F02
     END-IF.
*##2021/03/01 INSERT START
*    IF WK-SYORI =  2 OR 3
*       MOVE  TBL-REC       TO  WK-TBL-AREC
*    END-IF.
*##2021/03/01 INSERT END
     MOVE  DSP-SHOCD        TO  TBL-F031.
     MOVE  DSP-TAN1         TO  TBL-F0321.
     MOVE  DSP-TAN2         TO  TBL-F0322.
     MOVE  DSP-TAN3         TO  TBL-F0323.
     MOVE  DSP-SYKA         TO  TBL-F04.
     MOVE  DSP-GENTAN       TO  TBL-F05.
     MOVE  DSP-URITAN       TO  TBL-F06.
*    MOVE                   TO  TBL-F07.
     MOVE  DSP-TANA         TO  TBL-F08.
     MOVE  DSP-STAN         TO  TBL-F09.
*    MOVE                   TO  TBL-F10.
*    MOVE                   TO  TBL-F11.
*    MOVE                   TO  TBL-F12.
     MOVE  PARA-TANCDI(1:2) TO  TBL-F14.
     MOVE  SYS-DATE         TO  TBL-F99.
*##2021/03/01 INSERT START
*    IF WK-SYORI =  1
*       MOVE  TBL-REC       TO  WK-TBL-AREC
*    END-IF.
*##2021/03/01 INSERT END
*    処理モードにより追加・更新・削除
     EVALUATE  WK-SYORI
       WHEN  1
         MOVE  PARA-TANCDI(1:2) TO  TBL-F13
         MOVE  SYS-DATE         TO  TBL-F98
*##2021/03/01 START
         PERFORM  HSHOTBR-WRITE-SEC
*##2021/03/01 END
         WRITE  TBL-REC
       WHEN  2
*##2021/03/01 START
         PERFORM  HSHOTBR-WRITE-SEC
*##2021/03/01 END
         REWRITE  TBL-REC
       WHEN  3
         IF  HSHOTBL-INV-FLG = SPACE
*##2021/03/01 START
             PERFORM  HSHOTBD-WRITE-SEC
*##2021/03/01 END
             DELETE  HSHOTBL
         END-IF
     END-EVALUATE.
*##2019/12/05 END
*
*↓2021.12.21
*SUB商品変換テーブル更新処理
*   .
     IF WK-SYORI =  1
        MOVE  SPACE         TO  SUB-REC
        INITIALIZE              SUB-REC
        MOVE  PARA-TOKCDI   TO  SUB-F01
        MOVE  DSP-NFSYOC    TO  SUB-F02
        PERFORM  SUBTBLF-READ-SEC
        IF SUBTBLF-INV-FLG  =   SPACE   *>存在したら
           MOVE  2             TO  WK-SYORI-1    *>更新
        ELSE                            *>存在しなかったら
           MOVE  SPACE         TO  SUB-REC
           INITIALIZE              SUB-REC
           MOVE  PARA-TOKCDI   TO  SUB-F01
           MOVE  DSP-NFSYOC    TO  SUB-F02
           MOVE  1             TO  WK-SYORI-1    *>登録
        END-IF
     END-IF.
*
     IF WK-SYORI =  2
        MOVE  SPACE         TO  SUB-REC
        INITIALIZE              SUB-REC
        MOVE  PARA-TOKCDI   TO  SUB-F01
        MOVE  DSP-NFSYOC    TO  SUB-F02
        PERFORM  SUBTBLF-READ-SEC
        IF SUBTBLF-INV-FLG  =   SPACE   *>存在したら
           MOVE  2             TO  WK-SYORI-1    *>更新
        ELSE                            *>存在しなかったら
           MOVE  SPACE         TO  SUB-REC
           INITIALIZE              SUB-REC
           MOVE  PARA-TOKCDI   TO  SUB-F01
           MOVE  DSP-NFSYOC    TO  SUB-F02
           MOVE  1             TO  WK-SYORI-1    *>登録
        END-IF
     END-IF.
*
     IF WK-SYORI =  3
        MOVE  SPACE         TO  SUB-REC
        INITIALIZE              SUB-REC
        MOVE  PARA-TOKCDI   TO  SUB-F01
        MOVE  DSP-NFSYOC    TO  SUB-F02
        PERFORM  SUBTBLF-READ-SEC
        IF SUBTBLF-INV-FLG  =   SPACE   *>存在したら
           MOVE  3             TO  WK-SYORI-1    *>更新
        ELSE                            *>存在しなかったら
           GO                  TO  FILPRT-010
        END-IF
     END-IF.
     MOVE  DSP-SHOCD        TO  SUB-F031.
     MOVE  DSP-TAN1         TO  SUB-F0321.
     MOVE  DSP-TAN2         TO  SUB-F0322.
     MOVE  DSP-TAN3         TO  SUB-F0323.
     MOVE  DSP-SYKA         TO  SUB-F04.
     MOVE  DSP-GENTAN       TO  SUB-F05.
     MOVE  DSP-URITAN       TO  SUB-F06.
     MOVE  DSP-TANA         TO  SUB-F08.
     MOVE  DSP-STAN         TO  SUB-F09.
     MOVE  DSP-SHOCD        TO  SBM-F011.
     MOVE  DSP-TAN1         TO  SBM-F0121.
     MOVE  DSP-TAN2         TO  SBM-F0122.
     MOVE  DSP-TAN3         TO  SBM-F0123.
     PERFORM SUBMEIF-READ-SEC.
     IF    SUBMEIF-INV-FLG = SPACE
           MOVE  SBM-D01    TO  SUB-F17
           MOVE  SBM-D02    TO  SUB-F18
     ELSE
           MOVE  SPACE      TO  SUB-F17
                                SUB-F18
     END-IF.
     MOVE  PARA-BUMONCDI    TO  SUB-F96.
     MOVE  PARA-TANCDI(1:2) TO  SUB-F97.
     MOVE  SYS-DATE         TO  SUB-F98.
     MOVE  WK-TIME(1:6)     TO  SUB-F99.
 CCC.
*    処理モードにより追加・更新・削除
*****EVALUATE  WK-SYORI
     EVALUATE  WK-SYORI-1
       WHEN  1
         MOVE  PARA-BUMONCDI    TO  SUB-F92
         MOVE  PARA-TANCDI(1:2) TO  SUB-F93
         MOVE  SYS-DATE         TO  SUB-F94
         MOVE  WK-TIME(1:6)     TO  SUB-F95
         WRITE SUB-REC
       WHEN  2
         IF    SUBTBLF-INV-FLG = SPACE
               REWRITE SUB-REC
         ELSE
               MOVE  PARA-TOKCDI      TO  SUB-F01
               MOVE  DSP-NFSYOC       TO  SUB-F02
               MOVE  PARA-BUMONCDI    TO  SUB-F92
               MOVE  PARA-TANCDI(1:2) TO  SUB-F93
               MOVE  SYS-DATE         TO  SUB-F94
               MOVE  WK-TIME(1:6)     TO  SUB-F95
               WRITE SUB-REC
         END-IF
       WHEN  3
         IF    SUBTBLF-INV-FLG = SPACE
             DELETE  SUBTBLF
         END-IF
     END-EVALUATE.
*↑2021.12.21
 DDD.
*
 FILPRT-010.
     PERFORM  HEADDEL-SUB.
     MOVE  SPACE            TO  DSP-MEISAI.
     PERFORM  BODYDEL-SUB.
     MOVE  "2"              TO  MAIN-FLG.

 FILPRT-END.
     EXIT.
*--------------------------------------------------------------*
*    商品変換テーブル読込                                      *
*--------------------------------------------------------------*
 HSHOTBL-READ-SEC       SECTION.
*
     READ  HSHOTBL
           INVALID      MOVE "INV" TO HSHOTBL-INV-FLG
           NOT INVALID  MOVE SPACE TO HSHOTBL-INV-FLG
*##2021/03/01 START
                        MOVE TBL-REC  TO   WK-TBL-AREC
*##2021/03/01 END
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品名称マスタ読込                                       *
*--------------------------------------------------------------*
 HMEIMS-READ-SEC        SECTION.
*
     READ  HMEIMS
           INVALID      MOVE "INV" TO HMEIMS-INV-FLG
           NOT INVALID  MOVE SPACE TO HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*
*↓2021.12.21
*--------------------------------------------------------------*
*    SUB商品名称マスタ読込                                     *
*--------------------------------------------------------------*
 SUBMEIF-READ-SEC       SECTION.
*
     READ  SUBMEIF
           INVALID      MOVE "INV" TO SUBMEIF-INV-FLG
           NOT INVALID  MOVE SPACE TO SUBMEIF-INV-FLG
     END-READ.
*
 SUBMEIF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    SUB商品変換ＴＢＬ読込                                     *
*--------------------------------------------------------------*
 SUBTBLF-READ-SEC       SECTION.
*
     READ  SUBTBLF
           INVALID      MOVE "INV" TO SUBTBLF-INV-FLG
           NOT INVALID  MOVE SPACE TO SUBTBLF-INV-FLG
     END-READ.
*
 SUBTBLF-READ-EXIT.
     EXIT.
*↑2021.12.21
*
*--------------------------------------------------------------*
*    担当者マスタ読込                                          *
*--------------------------------------------------------------*
 HTANMS-READ-SEC        SECTION.
*
     READ  HTANMS
           INVALID      MOVE "INV" TO HTANMS-INV-FLG
           NOT INVALID  MOVE SPACE TO HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    作場マスタ読込　　                                        *
*--------------------------------------------------------------*
 SAKUBAF-READ-SEC       SECTION.
*
     READ  SAKUBAF
           INVALID      MOVE "INV" TO SAKUBAF-INV-FLG
           NOT INVALID  MOVE SPACE TO SAKUBAF-INV-FLG
     END-READ.
*
 SAKUBAF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    条件ファイル読込　                                        *
*--------------------------------------------------------------*
 HJYOKEN-READ-SEC       SECTION.
*
     READ  HJYOKEN
           INVALID      MOVE "INV" TO HJYOKEN-INV-FLG
           NOT INVALID  MOVE SPACE TO HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*   倉庫マスタ読込                       2019/12/05 INSERT     *
*--------------------------------------------------------------*
 ZSOKMS-READ-SEC        SECTION.
*
     READ  ZSOKMS
           INVALID      MOVE "INV" TO ZSOKMS-INV-FLG
           NOT INVALID  MOVE SPACE TO ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
**********************************************************
*                     履歴ファイル書き出し               *
**********************************************************
 HSHOTBR-WRITE-SEC          SECTION.
     INITIALIZE  STR-REC.
     IF  WK-SYORI =  2
         MOVE    WK-TBL-AREC  TO  STR-REC(16:85)
     END-IF.
     MOVE   TBL-REC            TO  STR-REC(101:85).
     MOVE   SYS-DATE           TO  STR-F01.
     MOVE   WK-TIME(1:4)       TO  STR-F02.
     MOVE   PARA-TANCDI(1:2)   TO  STR-F03.
     MOVE   WK-SYORI           TO  STR-F04.
     IF     WK-TBL-AREC(1:71) NOT = TBL-REC(1:71)
*****DISPLAY "WK-TBL-AREC = "  WK-TBL-AREC(1:71)  UPON CONS
*****DISPLAY "TBL-REC     = "  TBL-REC(1:71)      UPON CONS
*****DISPLAY "F01 = " TBL-F01 " : " TBL-F01  UPON CONS
*****DISPLAY "F02 = " TBL-F02 " : " TBL-F02  UPON CONS
*****DISPLAY "F03 = " TBL-F03 " : " TBL-F03  UPON CONS
*****DISPLAY "F04 = " TBL-F04 " : " TBL-F04  UPON CONS
*****DISPLAY "F05 = " TBL-F05 " : " TBL-F05  UPON CONS
*****DISPLAY "F06 = " TBL-F06 " : " TBL-F06  UPON CONS
*****DISPLAY "F07 = " TBL-F07 " : " TBL-F07  UPON CONS
*****DISPLAY "F08 = " TBL-F08 " : " TBL-F08  UPON CONS
*****DISPLAY "F09 = " TBL-F09 " : " TBL-F09  UPON CONS
*****DISPLAY "F10 = " TBL-F10 " : " TBL-F10  UPON CONS
*****DISPLAY "F11 = " TBL-F11 " : " TBL-F11  UPON CONS
*****DISPLAY "F12 = " TBL-F12 " : " TBL-F12  UPON CONS
*****DISPLAY "F13 = " TBL-F13 " : " TBL-F13  UPON CONS
*****DISPLAY "F14 = " TBL-F14 " : " TBL-F14  UPON CONS
*****DISPLAY "F98 = " TBL-F98 " : " TBL-F98  UPON CONS
*****DISPLAY "F99 = " TBL-F99 " : " TBL-F99  UPON CONS
            WRITE  STR-REC
     END-IF.
 HSHOTBR-WRITE-EXIT.
     EXIT.
**********************************************************
*    履歴ファイル更新（削除の場合）
**********************************************************
 HSHOTBD-WRITE-SEC          SECTION.
     INITIALIZE  STR-REC.
     MOVE   WK-TBL-AREC        TO  STR-REC(16:85).
     MOVE   PARA-TOKCDI        TO  STR-FB01.
     MOVE   SYS-DATE           TO  STR-F01.
     MOVE   WK-TIME(1:4)       TO  STR-F02.
     MOVE   PARA-TANCDI(1:2)   TO  STR-F03.
     MOVE   "3"                TO  STR-F04.
     WRITE  STR-REC.
 HSHOTBD-WRITE-EXIT.
     EXIT.
****************************************************************
*    3.0  終了処理                                             *
****************************************************************
 END-SEC                SECTION.
     CLOSE  DSPF.
     CLOSE  NFSHOMS.
     CLOSE  HSHOTBL  HMEIMS  HTANMS  SAKUBAF  HJYOKEN.
*##2019/12/05 INSERT START
     CLOSE  ZSOKMS.
*##2019/12/05 END
*↓2021.12.21
     CLOSE  SUBMEIF  SUBTBLF.
*↑2021.12.21
*##2021/03/01 START
*チェックリスト自動出力の為
     MOVE    SYS-DATE      TO        PARA-NDATE.
     MOVE    WK-TIME(1:4)  TO        PARA-NTIME.
*##2021/03/01 END
*
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
