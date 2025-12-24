# SFU0010I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SFU0010I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　在庫管理システム　　　　　　　　　　　*
*    モジュール名　　：　本社振替依頼入力　　　　　　　　　　　*
*    作成日　　　　　：　00/06/16                              *
*    作成者　　　　　：　NAV                                   *
*    更新日　　　　　：　**/**/**                              *
*    更新者　　　　　：　                                      *
*    更新内容　　　　：　                                      *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU0010I.
 AUTHOR.                Y.Y.
 DATE-WRITTEN.          00/06/16.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 振替依頼ファイル >>-*
     SELECT   FUIRAIF   ASSIGN    TO        DA-01-VI-FUIRAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       FIR-F01  FIR-F02
                                            FIR-F03
                        FILE      STATUS    FIR-ST1.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
*----<< 商品名称マスタ >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       MEI-F011   MEI-F0121
                                            MEI-F0122  MEI-F0123
                        FILE      STATUS    MEI-ST1.
*----<< 倉庫マスタ >>-*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    SOK-ST1.
*----<< 画面ファイル >>-*
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        SYMBOLIC  DESTINATION        "DSP"
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING  MODE    DSP-PRO
                        SELECTED  FUNCTION  DSP-FNC
                        FILE      STATUS    DSP-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 振替依頼ファイル >>-*
 FD  FUIRAIF            LABEL     RECORD     IS  STANDARD.
     COPY     FUIRAIF   OF   XFDLIB    JOINING   FIR  AS PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            LABEL     RECORD     IS  STANDARD.
     COPY     JYOKEN1   OF   XFDLIB    JOINING   JYO  AS PREFIX.
*----<< 商品名称マスタ >>-*
 FD  HMEIMS             LABEL     RECORD     IS  STANDARD.
     COPY     MEIMS1    OF   XFDLIB    JOINING   MEI  AS PREFIX.
*----<< 倉庫マスタ >>-*
 FD  ZSOKMS             LABEL     RECORD     IS  STANDARD.
     COPY     ZSOKMS1   OF   XFDLIB    JOINING   SOK  AS PREFIX.
*----<< 画面ファイル >>-*
 FD  DSPFILE.
     COPY     FFU00101  OF   XMDLIB.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "SFU0010I".
 01  DATE-FLG                PIC  9(01)     VALUE   ZERO.
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  DSP-CNTL.
     03  DSP-FMT             PIC  X(08).
     03  DSP-GRP             PIC  X(08).
     03  DSP-PRO             PIC  X(02).
     03  DSP-FNC             PIC  X(04).
     03  DSP-ST1             PIC  X(02).
     03  DSP-ST2             PIC  X(04).
     03  DSP-CON             PIC  X(06).
     03  WK-GRP.
         05  WK-BODY             PIC  X(04).
         05  WK-LINE             PIC  9(02).
         05  FILLER              PIC  X(02).
 01  STATUS-AREA.
     03  IN-DATA             PIC  X(01).
     03  FIR-STATUS.
         05  FIR-ST1         PIC  X(02).
         05  FIR-ST2         PIC  X(04).
     03  JYO-STATUS.
         05  JYO-ST1         PIC  X(02).
         05  JYO-ST2         PIC  X(04).
     03  MEI-STATUS.
         05  MEI-ST1         PIC  X(02).
         05  MEI-ST2         PIC  X(04).
     03  SOK-STATUS.
         05  SOK-ST1         PIC  X(02).
         05  SOK-ST2         PIC  X(04).
*画面退避用
     COPY   FFU00101  OF XMDLIB  JOINING   SAV  AS   PREFIX.
*----<< ﾜｰｸ ｴﾘｱ >>----
 01  WK-AREA.
     03  WK-SOKCD            PIC  X(02)  VALUE  SPACE.
     03  WK-DSOKCD           PIC  X(02)  VALUE  SPACE.
     03  WK-EIGYO            PIC  X(02)  VALUE  SPACE.
     03  WK-EIGMEI           PIC  N(15)  VALUE  SPACE.
 01  WK-HINMEI.
     03  WK-HINMEI1          PIC  N(15).
     03  WK-HINMEI2          PIC  N(15).
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｶﾞｲﾀﾞﾝｽ ｴﾘｱ >>-*
 01  GUIDE-AREA.
     03  G001                PIC  N(30)  VALUE
         NC"_取消_終了".
     03  G002                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り".
     03  G003                PIC  N(30)  VALUE
         NC"_取消_終了_項目戻り_前レコード_次レコード".
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
                    NC"表示中のＰＦキー以外は使用できません".
     03  MSG02               PIC  N(20)  VALUE
                    NC"伝票_に誤りがあります".
     03  MSG03               PIC  N(20)  VALUE
                    NC"伝票区分に誤りがあります．".
     03  MSG04               PIC  N(20)  VALUE
                    NC"出荷希望日の入力に誤りがあります．".
     03  MSG05               PIC  N(20)  VALUE
                    NC"場所に誤りがあります．".
     03  MSG06               PIC  N(20)  VALUE
                    NC"数量に誤りがあります．".
     03  MSG07               PIC  N(20)  VALUE
                    NC"ＹまたはＨを入力して下さい．".
     03  MSG08               PIC  N(20)  VALUE
                   NC"入力された伝票_は既に登録されています．".
     03  MSG09               PIC  N(20)  VALUE
                    NC"入力された倉庫は存在しません．".
     03  MSG10               PIC  N(20)  VALUE
                    NC"入力された伝票_は存在しません．".
     03  MSG11               PIC  N(20)  VALUE
                    NC"削除区分に誤りがあります．".
     03  MSG12               PIC  N(20)  VALUE
                    NC"入力された商品は未登録です．".
     03  MSG13               PIC  N(20)  VALUE
                    NC"次レコードは存在しません．".
     03  MSG14               PIC  N(20)  VALUE
                    NC"前レコードは存在しません．".
     03  MSG15               PIC  N(20)  VALUE
                    NC"処理区分に誤りがあります．".
     03  MSG16               PIC  N(20)  VALUE
                    NC"営業所を入力して下さい．".
     03  MSG17               PIC  N(20)  VALUE
                    NC"明細を入力して下さい．".
     03  MSG18               PIC  N(20)  VALUE
                    NC"振替元倉庫を入力して下さい。".
     03  MSG19               PIC  N(20)  VALUE
                    NC"振替先倉庫を入力して下さい。".
     03  MSG20               PIC  N(20)  VALUE
                    NC"依頼済のデータは修正できません。".
     03  MSG21               PIC  N(20)  VALUE
                    NC"依頼区分に誤りがあります。".
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       21.
*----<< ｼｽﾃﾑ ﾋﾂﾞｹ･ｼﾞｶﾝ ｴﾘｱ >>----
 01  WK-SYS-DATE             PIC  9(08).
*
 01  WK-IRI-DATE             PIC  9(08).
 01  SYS-DATE                PIC  9(06).
 01  FILLER                  REDEFINES  SYS-DATE.
     03  SYS-YY              PIC  9(02).
     03  SYS-MM              PIC  9(02).
     03  SYS-DD              PIC  9(02).
 01  SYS-DATE2               PIC  9(08).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*----<< ｲﾝﾃﾞｯｸｽ >>----
 01  INDEXES.
     03  I                   PIC  9(02).
     03  J                   PIC  9(02).
     03  L                   PIC  9(02).
     03  IXB                 PIC  9(02).
     03  IXC                 PIC  9(02).
     03  IXD                 PIC  9(02).
*----<< ﾌﾗｸﾞ ｴﾘｱ >>----
 01  FLAGS.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  INV-FLG             PIC  9(01)  VALUE  ZERO.
     03  FIR-FLG             PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG             PIC  9(02)  VALUE  ZERO.
     03  SYR-FLG             PIC  9(02)  VALUE  ZERO.
     03  DEL-FLG             PIC  9(01)  OCCURS  6.
*----<< ｶｳﾝﾄ ｴﾘｱ >>----
 01  COUNTERS.
     03  MAX-LINE            PIC  9(02)  VALUE  ZERO.
     03  GYO-CNT             PIC  9(01)  VALUE  ZERO.
*----<< ﾋﾝﾀﾝ ﾜｰｸ ｴﾘｱ >>----
 01  WK-HINT-AREA.
     03  WK-HINTI-X.
         05  WK-HINTI        PIC  X(01)  OCCURS  5.
     03  WK-HINTO-X.
         05  WK-HINTO        PIC  X(01)  OCCURS  5.
     03  WK-HINT             PIC  X(05).
*----<< ｷｰ ﾜｰｸ ｴﾘｱ >>----
 01  WK-KEY-AREA.
     03  WK-F02              PIC  9(07).
     03  WK-SOK              PIC  X(02).
*
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 LINKAGE        SECTION.
 01  LINK-AREA.
     03  LINK-IN.
         05  LI-WSMEI        PIC  X(08).
*
******************************************************************
 PROCEDURE           DIVISION    USING   LINK-AREA.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------  振替依頼ファイル  --------------------------------*
 FIR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   FUIRAIF.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"振替依頼ファイル異常！"
              "ST1=" FIR-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     STOP     RUN.
*----------  条件ファイル　---------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     STOP     RUN.
*----------   商品名称マスタ　 --------------------------------*
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HMEIMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"商品名称マスタ異常！"
              "ST1=" MEI-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     STOP     RUN.
*----------   倉庫マスタ　-----------------------------------*
 AOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   ZSOKMS.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"倉庫マスタ異常！"
              "ST1=" SOK-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     STOP     RUN.
*----------    表示ファイル -----------------------------------*
 DSP-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   DSPFILE.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "### " PGM-ID " " NC"表示ファイル異常！"
              "ST1=" DSP-ST1                 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"  UPON STAT.
*
     ACCEPT   IN-DATA        FROM STAT.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   0     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG  =  "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1     ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 INIT-RTN           SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE2.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " START "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
*画面日付・時刻編集
     MOVE      SYS-DATE2(1:4)     TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)     TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)     TO   HEN-DATE-DD.
     MOVE      SYS-TIME(1:2)      TO   HEN-TIME-HH.
     MOVE      SYS-TIME(3:2)      TO   HEN-TIME-MM.
     MOVE      SYS-TIME(5:2)      TO   HEN-TIME-SS.
*フラグ初期化
     INITIALIZE         FLAGS.
*ファイル　オープン
     OPEN     I-O       FUIRAIF.
     OPEN     I-O       HJYOKEN.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     I-O       DSPFILE.
*場所取得
     MOVE     "65"                TO   JYO-F01.
     MOVE     LI-WSMEI            TO   JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF  INV-FLG  =  1
         DISPLAY   "HJYOKEN INV KEY=65"  UPON STAT
         MOVE      "END"     TO   END-FLG
         GO   TO   INIT-EXIT
     END-IF.
     MOVE     JYO-F14(1:2)        TO   WK-SOKCD  WK-EIGYO.
     MOVE     JYO-F15(1:2)        TO   WK-DSOKCD.
     REWRITE  JYO-REC.
*実行営業所名取得
     MOVE     JYO-F14(1:2)        TO   SOK-F01.
     PERFORM  SOK-READ-RTN.
     IF  INV-FLG    =    1
         MOVE      "END"     TO   END-FLG
         GO   TO   INIT-EXIT
     ELSE
         MOVE      SOK-F02   TO   WK-EIGMEI
     END-IF.
*
     MOVE     0                   TO   SYR-FLG.
 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL    2.0  ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 MAIN-RTN           SECTION.
*
     EVALUATE    SYR-FLG
*                       初期処理
         WHEN    0      PERFORM   DSP-INIT-RTN
*                       処理区分入力
         WHEN    1      PERFORM   DSP-SRKBN-RTN
         WHEN    2      PERFORM   DSP-HEAD-RTN
         WHEN    3      PERFORM   DSP-IRINO-RTN
         WHEN    4      PERFORM   DSP-BODY-RTN
         WHEN    90     PERFORM   DSP-KAKNIN-RTN
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   3.0   ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 END-RTN            SECTION.
* 各ファイルをクローズする
     CLOSE              FUIRAIF
                        HJYOKEN
                        HMEIMS
                        ZSOKMS
                        DSPFILE.
*
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
     DISPLAY  "*** " PGM-ID " END   "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ***"  UPON STAT.
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL    ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 DSP-INIT-RTN           SECTION.
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     MOVE     SPACE          TO   FFU00101.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FFU00101"     TO   DSP-FMT.
     MOVE     "ALLF"         TO   DSP-GRP.
* 処理区分の入力を行う
     MOVE     1              TO   SYR-FLG.
 DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2.1    処理区分　入力              SYR-FLG = 1     *
*--------------------------------------------------------------*
 DSP-SRKBN-RTN          SECTION.
*----- 営業所プロテクト ----*
     IF    WK-SOKCD     =  "01"  AND
           WK-DSOKCD    =  "01"
           CONTINUE
     ELSE
           MOVE   "X"        TO  EDIT-STATUS  OF  EIGYO
     END-IF.
*
     MOVE     G001           TO   GUIDE.
     MOVE     "GRP001"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
* アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"   MOVE      0         TO   SYR-FLG
     WHEN     "F005"   MOVE     "END"      TO   END-FLG
     WHEN     "E000"   PERFORM   CHK-SRKBN-RTN
     WHEN     OTHER    MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-SRKBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2.1.1  処理区分　チェック　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-SRKBN-RTN          SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
*
     IF  WK-SOKCD       =   "01"  AND
         WK-DSOKCD      =   "01"
         IF   EIGYO     =    SPACE
              IF   ERR-FLG   =    ZERO
                   MOVE  16  TO   ERR-FLG
              END-IF
              MOVE     "R"   TO   EDIT-OPTION OF EIGYO
         ELSE
              MOVE      EIGYO     TO   WK-EIGYO
              MOVE      EIGYO     TO   SOK-F01
              PERFORM   SOK-READ-RTN
              IF  INV-FLG    =    1
                  IF    ERR-FLG   =    0
                        MOVE      9    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF EIGMEI
                  END-IF
                  MOVE "R"   TO   EDIT-OPTION  OF  EIGYO
              END-IF
              MOVE      SOK-F02   TO   EIGMEI  WK-EIGMEI
         END-IF
     END-IF.
*
     IF  SRKBN     IS   NOT  NUMERIC
         MOVE      0    TO   SRKBN
     END-IF.
     EVALUATE    SRKBN
         WHEN    1
                 MOVE  NC"登録"    TO  SRMEI
                 PERFORM  CHK-IRINO-RTN
                 MOVE  2           TO  SYR-FLG
         WHEN    2
                 MOVE  NC"修正"    TO  SRMEI
                 MOVE  3           TO  SYR-FLG
         WHEN    3
                 MOVE  NC"削除"    TO  SRMEI
                 MOVE  3           TO  SYR-FLG
         WHEN    9
                 MOVE  NC"照会"    TO  SRMEI
                 MOVE  3           TO  SYR-FLG
         WHEN    OTHER
                 IF    ERR-FLG     =   ZERO
                       MOVE  15    TO  ERR-FLG
                       MOVE  "C"   TO  EDIT-CURSOR OF SRKBN
                 END-IF
                 MOVE  "R"         TO  EDIT-OPTION OF SRKBN
     END-EVALUATE.
 CHK-SRKBN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＲＰ００３　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-HEAD-RTN      SECTION.
     MOVE     G002           TO   GUIDE.
     MOVE     "GRP003"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              IF   SRKBN     =   2
                   MOVE   3       TO   SYR-FLG
              ELSE
                   MOVE   1       TO   SYR-FLG
              END-IF
              PERFORM  CLR-HEAD-RTN
     WHEN     "E000"
              PERFORM  CHK-HEAD-RTN
              IF     ERR-FLG   =   ZERO
                     MOVE   4        TO  SYR-FLG
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      振替依頼日チェック                          *
*--------------------------------------------------------------*
 CHK-DATE-RTN        SECTION.
*
*振替依頼日
     IF  SYUYMD    IS   NOT NUMERIC
         MOVE      ZERO      TO   SYUYMD
     END-IF.
*日付論理チェック
     MOVE     "1"                 TO   LINK-IN-KBN.
     MOVE     SYUYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET   NOT =  ZERO
         IF  ERR-FLG       =   0
             MOVE   4          TO   ERR-FLG
         END-IF
         MOVE   "C"        TO   EDIT-CURSOR  OF  SYUYMD
         MOVE   "R"        TO   EDIT-OPTION  OF  SYUYMD
         GO      TO        CHK-DATE-EXT
     END-IF.
*
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYUYMD              TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"     USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-IRI-DATE.
*
     IF  SYS-DATE2      >    WK-IRI-DATE
         IF  ERR-FLG    =    0
             MOVE       4    TO   ERR-FLG
             MOVE      "C"   TO   EDIT-CURSOR  OF  SYUYMD
         END-IF
         MOVE     "R"   TO   EDIT-OPTION  OF  SYUYMD
     END-IF.
*
 CHK-DATE-EXT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッダ入力　　チェック　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-HEAD-RTN      SECTION.
*属性クリア
     PERFORM  CLR-HEAD-RTN.
***** 入力チェック *****
*振替依頼日
     PERFORM   CHK-DATE-RTN.
*依頼区分
     IF  IRIKBN    =    SPACE  OR  "1"
         IF   IRIKBN    =    "1"
              MOVE      NC"依頼する"   TO   IRIMEI
         ELSE
              MOVE      SPACE          TO   IRIMEI
         END-IF
     ELSE
         MOVE      SPACE     TO   IRIMEI
         IF   ERR-FLG   =    ZERO
              MOVE      21   TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR  OF  IRIKBN
         END-IF
     END-IF.
*
*振替元倉庫
     IF  BASY01    =    SPACE
         IF   ERR-FLG   =    ZERO
              MOVE      18   TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR  OF  BASY01
         END-IF
         MOVE     "R"        TO   EDIT-OPTION  OF  BASY01
     END-IF.
* 場所１存在チェック
     IF  BASY01    NOT =     SPACE
         MOVE      BASY01    TO   SOK-F01
         PERFORM   SOK-READ-RTN
         IF  INV-FLG    =    1
             IF    ERR-FLG   =    0
                   MOVE      9    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR  OF  BASY01
             END-IF
             MOVE "R"   TO   EDIT-OPTION  OF  BASY01
         END-IF
         MOVE      SOK-F02   TO   BSYM1
     END-IF.
*振替先倉庫
     IF  BASY02    =    SPACE
         IF   ERR-FLG   =    ZERO
              MOVE      19   TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR  OF  BASY02
         END-IF
         MOVE     "R"        TO   EDIT-OPTION  OF  BASY02
     END-IF.
* 場所２存在チェック
     IF  BASY02    NOT =     SPACE
         MOVE      BASY02    TO   SOK-F01
         PERFORM   SOK-READ-RTN
         IF  INV-FLG    =    1
             IF    ERR-FLG   =    0
                   MOVE      9    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR  OF  BASY02
             END-IF
             MOVE "R"   TO   EDIT-OPTION  OF  BASY02
         END-IF
         MOVE      SOK-F02   TO   BSYM2
     END-IF.
*振替元・振替先が同じならエラー
     IF  BASY01    =    BASY02
         IF   ERR-FLG   =    0
              MOVE      5    TO   ERR-FLG
              MOVE     "C"   TO   EDIT-CURSOR  OF  BASY01
         END-IF
         MOVE     "R"   TO   EDIT-OPTION  OF  BASY01
         MOVE     "R"   TO   EDIT-OPTION  OF  BASY02
     END-IF.
*
 CHK-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票_　入力                                *
*--------------------------------------------------------------*
 DSP-IRINO-RTN     SECTION.
*
     MOVE     G003           TO   GUIDE.
     MOVE     "GRP002"       TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
*アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE    0           TO   SYR-FLG
     WHEN     "F005"
              MOVE    "END"       TO   END-FLG
     WHEN     "F006"
              MOVE    1           TO   SYR-FLG
              PERFORM  CLR-HEAD-RTN
              MOVE  ZERO    TO   FIR-FLG
     WHEN     "E000"
              PERFORM   CHK-IRINO-RTN
              IF     ERR-FLG   =   ZERO
                     IF   SRKBN     =   2
                          MOVE   2      TO   SYR-FLG
                     ELSE
                          MOVE  "Y"     TO   KAKNIN
                          MOVE  90      TO   SYR-FLG
                     END-IF
              ELSE
                     MOVE  ZERO    TO   FIR-FLG
              END-IF
     WHEN     "F011"
              PERFORM   MAE-REC-RTN
**            IF     ERR-FLG   =   ZERO
**                   IF   SRKBN     =   2
**                        MOVE   2      TO   SYR-FLG
**                   ELSE
**                        MOVE  "Y"     TO   KAKNIN
**                        MOVE  90      TO   SYR-FLG
**                   END-IF
**            END-IF
     WHEN     "F012"
              PERFORM   TUGI-REC-RTN
**            IF     ERR-FLG   =   ZERO
**                   IF   SRKBN     =   2
**                        MOVE   2      TO   SYR-FLG
**                   ELSE
**                        MOVE  "Y"     TO   KAKNIN
**                        MOVE  90      TO   SYR-FLG
**                   END-IF
**            END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-IRINO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      伝票_　自動採番　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 IRINO-SET-RTN           SECTION.
* 伝票_を自動採番する
     MOVE     "61"           TO  JYO-F01.
     MOVE     "FUIRAI"       TO  JYO-F02.
     PERFORM  JYO-RD-RTN.
     IF       INV-FLG         =  1
              DISPLAY   "HJYOKEN INV KEY=61 FUIRAI"  UPON  STAT
              STOP   RUN
     END-IF.
     MOVE     JYO-F04        TO   IRINO.
*
     IF       IRINO     =    9999999
              MOVE      1    TO   JYO-F04
     ELSE
              ADD       1    TO   JYO-F04
     END-IF.
     REWRITE  JYO-REC.
*
     IF     DATE-FLG   =     ZERO
            MOVE   SYS-DATE         TO   SYUYMD
     ELSE
            MOVE   ZERO             TO   DATE-FLG
     END-IF.
*
 IRINO-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      存在チェック処理（伝票_）                  *
*--------------------------------------------------------------*
 CHK-IRINO-RTN           SECTION.
*全レコード解放（排他制御用）
     CLOSE    FUIRAIF.
     OPEN     I-O   FUIRAIF.
* ヘッド部属性クリア
     PERFORM   CLR-HEAD-RTN.
*
     IF       IRINO      IS  NOT  NUMERIC
              MOVE  0        TO  IRINO
     END-IF.
* 登録時，伝票_を自動採番する
     IF       SRKBN           =  1
              PERFORM    IRINO-SET-RTN
     END-IF.
* 振替依頼ファイル，存在チェック
     MOVE     WK-EIGYO       TO   FIR-F01.
     MOVE     IRINO          TO   FIR-F02.
     MOVE     0              TO   FIR-F03.
     START    FUIRAIF   KEY  >=   FIR-F01  FIR-F02  FIR-F03
         INVALID    KEY
                MOVE   1       TO   FIR-FLG
         NOT INVALID KEY
                PERFORM        FIR-READ-RTN
     END-START.
* 登録以外の時
     IF      (FIR-FLG         =   1)  AND
             (SRKBN       NOT =   1)
             MOVE   2        TO   ERR-FLG
             MOVE   "C"      TO   EDIT-CURSOR  OF  IRINO
             MOVE   "R"      TO   EDIT-OPTION  OF  IRINO
             GO  TO   CHK-IRINO-EXIT
     END-IF.
* 登録の時
     IF      (FIR-FLG         =   0)  AND
             (SRKBN           =   1)
             MOVE   8        TO   ERR-FLG
             MOVE   "C"      TO   EDIT-CURSOR  OF  IRINO
             MOVE   "R"      TO   EDIT-OPTION  OF  IRINO
             GO  TO   CHK-IRINO-EXIT
     END-IF.
     IF      SRKBN            =   1
             GO  TO   CHK-IRINO-EXIT
     END-IF.
* 該当データを画面にセットする
     MOVE    SRKBN           TO   SAV-SRKBN.
     MOVE    SRMEI           TO   SAV-SRMEI.
     MOVE    HEAD1           TO   SAV-HEAD1.
     MOVE    SPACE           TO   FFU00101.
     MOVE    SAV-SRKBN       TO   SRKBN.
     MOVE    SAV-SRMEI       TO   SRMEI.
     MOVE    SAV-HEAD1       TO   HEAD1.
     PERFORM     FIR-DSP-RTN.
*
 CHK-IRINO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      前レコードチェック処理                      *
*--------------------------------------------------------------*
 MAE-REC-RTN            SECTION.
*全レコード解放（排他制御用）
     CLOSE    FUIRAIF.
     OPEN     I-O   FUIRAIF.
* 属性クリア
     MOVE     ZERO      TO  FIR-FLG.
     PERFORM   CLR-HEAD-RTN.
*
     IF       IRINO      IS  NOT  NUMERIC
              MOVE  0        TO  IRINO
     END-IF.
* 伝票_存在チェック
     MOVE     WK-EIGYO       TO  FIR-F01.
     MOVE     IRINO          TO  FIR-F02.
     MOVE     0              TO  FIR-F03.
     START    FUIRAIF   KEY  IS  <  FIR-F01  FIR-F02  FIR-F03
                 WITH   REVERSED  ORDER
         INVALID  KEY
              MOVE  1        TO  FIR-FLG
         NOT  INVALID  KEY
              PERFORM  FIR-READ1-RTN
              IF  FIR-FLG           =  0
                  MOVE    FIR-F02     TO  WK-F02
                  MOVE    WK-EIGYO    TO  FIR-F01
                  MOVE    WK-F02      TO  FIR-F02
                  MOVE    0           TO  FIR-F03
                  START    FUIRAIF   KEY  >=   FIR-F01
                                               FIR-F02
                                               FIR-F03
                      INVALID    KEY
                           MOVE   1       TO   FIR-FLG
                      NOT INVALID KEY
                          PERFORM        FIR-READ1-RTN
                  END-START
              END-IF
     END-START.
     IF      FIR-FLG          =   1
             MOVE   14       TO   ERR-FLG
             MOVE   "C"      TO   EDIT-CURSOR  OF  IRINO
             MOVE   "R"      TO   EDIT-OPTION  OF  IRINO
             GO  TO          MAE-REC-EXIT
     END-IF.
* 該当データを画面にセット
     MOVE    SRKBN           TO   SAV-SRKBN.
     MOVE    SRMEI           TO   SAV-SRMEI.
     MOVE    HEAD1           TO   SAV-HEAD1.
     MOVE    SPACE           TO   FFU00101.
     MOVE    SAV-SRKBN       TO   SRKBN.
     MOVE    SAV-SRMEI       TO   SRMEI.
     MOVE    SAV-HEAD1       TO   HEAD1.
     PERFORM     FIR-DSP-RTN.
*
 MAE-REC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      次レコードチェック処理                      *
*--------------------------------------------------------------*
 TUGI-REC-RTN           SECTION.
*全レコード解放（排他制御用）
     CLOSE    FUIRAIF.
     OPEN     I-O   FUIRAIF.
* 属性クリア
     MOVE     ZERO      TO  FIR-FLG.
     PERFORM   CLR-HEAD-RTN.
*
     IF       IRINO      IS  NOT  NUMERIC
              MOVE  0        TO  IRINO
     END-IF.
* 登録処理の場合エラー
     IF       SRKBN           =  1
              MOVE  1        TO  ERR-FLG
              MOVE  "C"      TO  EDIT-CURSOR  OF  IRINO
              MOVE  "R"      TO  EDIT-OPTION  OF  IRINO
              GO    TO       TUGI-REC-EXIT
     END-IF.
* 伝票_存在チェック
     MOVE     WK-EIGYO       TO  FIR-F01.
     MOVE     IRINO          TO  FIR-F02.
     MOVE     99             TO  FIR-F03.
     START    FUIRAIF   KEY   >  FIR-F01  FIR-F02  FIR-F03
         INVALID  KEY
              MOVE  1        TO  FIR-FLG
         NOT  INVALID  KEY
              PERFORM  FIR-READ1-RTN
     END-START.
     IF  FIR-FLG          =   1
         MOVE   13       TO   ERR-FLG
         MOVE   "C"      TO   EDIT-CURSOR  OF  IRINO
         MOVE   "R"      TO   EDIT-OPTION  OF  IRINO
         IF  SRKBN            =   1
             MOVE    SRKBN           TO   SAV-SRKBN
             MOVE    SRMEI           TO   SAV-SRMEI
             MOVE    SPACE           TO   FFU00101
             MOVE    SAV-SRKBN       TO   SRKBN
             MOVE    SAV-SRMEI       TO   SRMEI
         END-IF
         GO  TO          TUGI-REC-EXIT
     END-IF.
* 該当データを画面にセット
     IF    SRKBN            =   1
           MOVE    SPACE           TO   SAV-HEAD1
     ELSE
           MOVE    HEAD1           TO   SAV-HEAD1
     END-IF.
     MOVE    SRKBN           TO   SAV-SRKBN.
     MOVE    SRMEI           TO   SAV-SRMEI.
     MOVE    SPACE           TO   FFU00101.
     MOVE    SAV-SRKBN       TO   SRKBN.
     MOVE    SAV-SRMEI       TO   SRMEI.
     MOVE    SAV-HEAD1       TO   HEAD1.
     PERFORM     FIR-DSP-RTN.
*
 TUGI-REC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3         画面へ表示　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 FIR-DSP-RTN            SECTION.
*データ表示
     PERFORM   FIR-TO-HEAD.
     PERFORM   VARYING  I  FROM  1  BY  1  UNTIL    I  >  6
                                     OR       FIR-FLG  =  1
            IF    IRINO     =  FIR-F02
                  ADD   1    TO   MAX-LINE
                  IF    I    =    FIR-F03
                        PERFORM   FIR-TO-BODY
                        PERFORM   FIR-READ-RTN
                        MOVE   0        TO   DEL-FLG(I)
                  ELSE
                        PERFORM   PRO-BODY-RTN
                        MOVE   1        TO   DEL-FLG(I)
                  END-IF
            END-IF
     END-PERFORM.
* 入力行以降 プロテクトセット
     IF  I      <=     6
         PERFORM    UNTIL    I   >   6
               PERFORM   PRO-BODY-RTN
               MOVE    1        TO   DEL-FLG(I)
               ADD     1        TO   I
         END-PERFORM
     END-IF.
 FIR-DSP-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＧＢＯＤＹ１　入力　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-BODY-RTN     SECTION.
*
     MOVE      G002          TO   GUIDE.
     MOVE     "BODY"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
* アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              IF     SRKBN     =    1  OR  2
                     MOVE      2         TO   SYR-FLG
              ELSE
                     MOVE      3         TO   SYR-FLG
              END-IF
              PERFORM  CLR-BODY-RTN
              MOVE   ZERO     TO   FIR-FLG
     WHEN     "E000"
              PERFORM  CLR-BODY-RTN
              MOVE   ZERO         TO   GYO-CNT
              PERFORM  CHK-BODY-RTN
                            VARYING  L  FROM 1  BY  1
                              UNTIL  L  >  6
              IF   ERR-FLG  =  ZERO
                   PERFORM  CLR-BODY-RTN
                   MOVE    BODY1           TO  SAV-BODY1
                   IF  SRKBN   =  1
                       PERFORM   GYO-TUME-RTN
                   END-IF
                   PERFORM   VARYING  I   FROM  1  BY  1
                               UNTIL  I      >  6
                       MOVE    SAV-MEISAI(I)   TO  MEISAI(I)
                   END-PERFORM
                   PERFORM   CLR-TAIL-RTN
                   MOVE      "Y"       TO   KAKNIN
                   MOVE      90        TO   SYR-FLG
                   PERFORM  CLR-BODY-RTN
              END-IF
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
 DSP-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       ＢＯＤＹ　チェック　　　　 　　　　　　　*
*--------------------------------------------------------------*
 CHK-BODY-RTN         SECTION.
* 未入力チェック
      IF   (HIN1(L)          =   SPACE)   AND
           (HIN2(L)          =   SPACE)   AND
           (HIN3(L)          =   SPACE)   AND
           (SYOCD(L)         =   SPACE)
            IF  SRKBN   =    1
                GO  TO  CHK-BODY-90
            ELSE
                IF (SURYOU(L)  NOT   NUMERIC ) OR
                   (SURYOU(L)        =   ZERO)
                    MOVE  SPACE      TO  MEISAI(L) SAV-MEISAI(L)
                    MOVE  1          TO  DEL-FLG(L)
                    GO  TO  CHK-BODY-90
                 END-IF
            END-IF
      END-IF.
* 明細件数のカウント
     ADD   1                    TO   GYO-CNT.
     MOVE  0                    TO   DEL-FLG(L).
*
*** 商品コード右詰処理
     PERFORM VARYING IXB    FROM  1   BY   1
             UNTIL   IXB    >     7
         PERFORM VARYING IXC    FROM  8   BY  -1
                 UNTIL   IXC    <     2
             IF  SYOCD (L)(IXC:1)   =  SPACE
                 COMPUTE IXD    =     IXC   -   1
                 MOVE  SYOCD (L)(IXD:1)   TO   SYOCD (L)(IXC:1)
                 MOVE  SPACE              TO   SYOCD (L)(IXD:1)
             END-IF
         END-PERFORM
     END-PERFORM.
**
     PERFORM     VARYING    IXB   FROM    1   BY   1
                 UNTIL      (IXB   >      7 ) OR
                 (SYOCD (L)(IXB:1) NOT =  SPACE)
         IF      SYOCD (L)(IXB:1)        =   SPACE
                 MOVE       "0"   TO      SYOCD (L)(IXB:1)
         END-IF
     END-PERFORM.
**
*品単数字右づめ変換
*５桁　頭空白詰
     IF  HIN1(L)  NOT = SPACE   AND
         HIN1(L)(5:1) = SPACE
         MOVE   HIN1(L)  TO  WK-HINTI-X
         MOVE   ZERO       TO  J
         PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
             IF  WK-HINTI(I)  NOT = SPACE
                 ADD    1   TO  J
                 MOVE   WK-HINTI(I)  TO  WK-HINTO(J)
             END-IF
         END-PERFORM
         MOVE   SPACE            TO  WK-HINT
         COMPUTE I = 6 - J
         MOVE   WK-HINTO-X(1:J)  TO  WK-HINT(I:J)
         MOVE   WK-HINT          TO  HIN1(L)
     END-IF.
*商品名取得
     MOVE   SYOCD(L)         TO  MEI-F011.
     MOVE   HIN1(L)          TO  MEI-F0121.
     MOVE   HIN2(L)          TO  MEI-F0122.
     MOVE   HIN3(L)          TO  MEI-F0123.
     PERFORM   MEI-READ-RTN.
     IF   INV-FLG          =   1
          IF   ERR-FLG       =   ZERO
               MOVE  12      TO  ERR-FLG
          END-IF
          MOVE  "C"          TO  EDIT-CURSOR  OF SYOCD(L)
          MOVE  "R"          TO  EDIT-OPTION  OF SYOCD(L)
          MOVE  "R"          TO  EDIT-OPTION  OF HIN1(L)
          MOVE  "R"          TO  EDIT-OPTION  OF HIN2(L)
          MOVE  "R"          TO  EDIT-OPTION  OF HIN3(L)
     ELSE
          MOVE  MEI-F021     TO  WK-HINMEI1
          MOVE  MEI-F022     TO  WK-HINMEI2
          MOVE  WK-HINMEI    TO  HINMEI(L)
     END-IF.
*数量チェック
     IF  SURYOU(L)     NOT  NUMERIC
         MOVE   ZERO   TO   SURYOU(L)
     END-IF.
     IF  SURYOU(L)        =   ZERO    AND
         SRKBN            =   1
         IF    ERR-FLG      =   ZERO
               MOVE   6     TO  ERR-FLG
         END-IF
         MOVE  "C"          TO  EDIT-CURSOR  OF SURYOU(L)
         MOVE  "R"          TO  EDIT-OPTION  OF SURYOU(L)
     END-IF.
*
* 登録時，全行未入力の場合１行目入力エラー
 CHK-BODY-90.
     IF    SRKBN   =   1      AND
           L       =   6      AND
           GYO-CNT =   ZERO
           MOVE   17            TO   ERR-FLG
           MOVE   "C"           TO   EDIT-CURSOR  OF  SYOCD(1)
           MOVE   "R"           TO   EDIT-OPTION  OF  SYOCD(1)
           MOVE   "R"           TO   EDIT-OPTION  OF  HIN1(1)
           MOVE   "R"           TO   EDIT-OPTION  OF  HIN2(1)
           MOVE   "R"           TO   EDIT-OPTION  OF  HIN3(1)
     END-IF.
 CHK-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEBEL  3       行番桁詰めチェック　　　　 　　　　　　　*
*--------------------------------------------------------------*
 GYO-TUME-RTN         SECTION.
* 登録時，空白行の行詰めを行う
     MOVE   ZERO       TO  J.
     MOVE   SPACE      TO  SAV-BODY1.
       PERFORM   VARYING  I  FROM  1   BY  1
                   UNTIL  I     >  6
         IF   (SYOCD(I)     NOT =   SPACE)
               ADD   1          TO   J
               MOVE  MEISAI(I)  TO   SAV-MEISAI(J)
         END-IF
       END-PERFORM.
 GYO-TUME-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　入力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DSP-KAKNIN-RTN    SECTION.
     MOVE      G002            TO   GUIDE.
     MOVE     "KAK001"         TO   WK-GRP.
     PERFORM  DSP-RD-RTN.
* アテンション判定
     EVALUATE DSP-FNC
     WHEN     "F004"
              MOVE      0         TO   SYR-FLG
     WHEN     "F005"
              MOVE      "END"     TO   END-FLG
     WHEN     "F006"
              IF   SRKBN     =   1
                   MOVE      4         TO   SYR-FLG
                   MOVE      SAV-BODY1 TO   BODY1
***                PERFORM   VARYING  I  FROM  1  BY  1
***                                               UNTIL  I  >  6
***                     MOVE  "X"     TO  EDIT-STATUS  OF DLKBN(I)
***                END-PERFORM
              END-IF
              IF   SRKBN     =    2
                   MOVE      4         TO   SYR-FLG
                   MOVE      SAV-BODY1 TO   BODY1
                   PERFORM  VARYING  I  FROM  1  BY  1
                                                 UNTIL  I  >  6
                      IF   (HIN1(I)   =   SPACE)  AND
                           (HIN2(I)   =   SPACE)  AND
                           (HIN3(I)   =   SPACE)  AND
                           (SYOCD(I)    =   SPACE)
                            PERFORM   PRO-BODY-RTN
                      END-IF
                  END-PERFORM
              END-IF
              IF     SRKBN     =     3   OR   9
                     MOVE      3         TO   SYR-FLG
              END-IF
              MOVE      ZERO      TO   FIR-FLG
              PERFORM   CLR-TAIL-RTN
     WHEN     "E000"
              PERFORM   CHK-KAKNIN-RTN
              IF   ERR-FLG  =  ZERO
                   IF  SRKBN   NOT = 9
                       PERFORM   FIR-UPDT-RTN
                   END-IF
                   PERFORM   KAKNIN-AFTER-RTN
              END-IF
              PERFORM   CLR-TAIL-RTN
              PERFORM   CLR-BODY-RTN
     WHEN     OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認　　　　チェック　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CHK-KAKNIN-RTN     SECTION.
     MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
     IF      (KAKNIN  NOT  =  "Y") AND
             (KAKNIN  NOT  =  "H") AND
             (KAKNIN  NOT  =  "B")
              MOVE      7         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KAKNIN
     END-IF.
*
     IF      (KAKNIN       =  "B") AND
             (SRKBN    NOT =  1)
              MOVE      7         TO   ERR-FLG
              MOVE     "R"        TO   EDIT-OPTION OF KAKNIN
     END-IF.
 CHK-KAKNIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      振替依頼更新　処理                          *
*--------------------------------------------------------------*
 FIR-UPDT-RTN          SECTION.
*
     EVALUATE   SRKBN
         WHEN   1      PERFORM   FIR-WRITE
         WHEN   2      PERFORM   FIR-REWRITE
         WHEN   3      PERFORM   FIR-DELETE
     END-EVALUATE.
*
 FIR-UPDT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      確認後画面　処理　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 KAKNIN-AFTER-RTN          SECTION.
*
     EVALUATE    KAKNIN
         WHEN    "Y"
                 MOVE   SRKBN     TO   SAV-SRKBN
                 MOVE   SRMEI     TO   SAV-SRMEI
                 MOVE   SPACE     TO   FFU00101
                 MOVE   SAV-SRKBN TO   SRKBN
                 MOVE   SAV-SRMEI TO   SRMEI
                 IF    SRKBN             =   1
                       PERFORM   IRINO-SET-RTN
                       MOVE   2         TO   SYR-FLG
                 ELSE
                       MOVE   3         TO   SYR-FLG
                 END-IF
*
         WHEN    "H"
                 IF    SRKBN             =   1
                       MOVE   SRKBN      TO   SAV-SRKBN
                       MOVE   SRMEI      TO   SAV-SRMEI
                       MOVE   HEAD1      TO   SAV-HEAD1
                       MOVE   SPACE      TO   FFU00101
                       MOVE   SAV-SRKBN  TO   SRKBN
                       MOVE   SAV-SRMEI  TO   SRMEI
                       MOVE   SAV-HEAD1  TO   HEAD1
                       MOVE   1          TO   DATE-FLG
                       PERFORM   IRINO-SET-RTN
                       MOVE   4          TO   SYR-FLG
                 ELSE
                       PERFORM   TUGI-REC-RTN
                       MOVE   SPACE      TO   BODY1
                       MOVE   3          TO   SYR-FLG
                 END-IF
*
         WHEN    "B"
                 MOVE   SRKBN      TO  SAV-SRKBN
                 MOVE   SRMEI      TO  SAV-SRMEI
                 MOVE   BODY1      TO  SAV-BODY1
                 MOVE   SYUYMD     TO  SAV-SYUYMD
                 MOVE   SPACE      TO  FFU00101
                 MOVE   SAV-BODY1  TO  BODY1
                 MOVE   SAV-SRKBN  TO  SRKBN
                 MOVE   SAV-SRMEI  TO  SRMEI
                 MOVE   SAV-SYUYMD TO  SYUYMD
                 MOVE   1          TO  DATE-FLG
                 PERFORM   IRINO-SET-RTN
                 MOVE   2          TO  SYR-FLG
     END-EVALUATE.
 KAKNIN-AFTER-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     振替依頼ファイル登録処理　　　　　　　　　　 *
*--------------------------------------------------------------*
 FIR-WRITE              SECTION.
*振替依頼ファイル登録
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
       IF  (SYOCD (I)      NOT  =  SPACE)
           PERFORM   DSP-TO-NYU
           WRITE     FIR-REC
       END-IF
     END-PERFORM.
 FIR-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     振替依頼ファイル更新処理　　　　　　　　　　 *
*--------------------------------------------------------------*
 FIR-REWRITE            SECTION.
*振替依頼ファイル更新
     MOVE   WK-EIGYO         TO   FIR-F01.
     MOVE   IRINO            TO   FIR-F02.
     MOVE   0                TO   FIR-F03.
     START    FUIRAIF   KEY  >=   FIR-F01  FIR-F02  FIR-F03
       INVALID    KEY
              MOVE   1       TO   FIR-FLG
       NOT INVALID KEY
              PERFORM        FIR-READ-RTN
     END-START.
     PERFORM  VARYING  I  FROM  1  BY  1
                UNTIL  I     >  6  OR  FIR-FLG  =  1
              IF  I            =  FIR-F03
                  IF    DEL-FLG(I)    =   ZERO
                        PERFORM    DSP-TO-NYU
                        REWRITE    FIR-REC
                  ELSE
                        DELETE   FUIRAIF  RECORD
                  END-IF
                  PERFORM    FIR-READ-RTN
              END-IF
     END-PERFORM.
 FIR-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     振替依頼ファイル削除                         *
*--------------------------------------------------------------*
 FIR-DELETE             SECTION.
* 削除モードで処理した場合，削除フラグに１をセット
     MOVE     0              TO   FIR-FLG.
     MOVE     SPACE          TO   FIR-REC.
     INITIALIZE                   FIR-REC.
     MOVE     WK-EIGYO       TO   FIR-F01.
     MOVE     IRINO          TO   FIR-F02.
     MOVE     0              TO   FIR-F03.
     START    FUIRAIF   KEY  >=   FIR-F01  FIR-F02  FIR-F03
       INVALID    KEY
              MOVE   1       TO   FIR-FLG
       NOT INVALID KEY
              PERFORM        FIR-READ-RTN
     END-START.
     PERFORM  VARYING  I  FROM  1  BY  1
                UNTIL  FIR-FLG  =  1  OR  I   >  6
              IF   I            =  FIR-F03
                   DELETE     FUIRAIF
                   PERFORM    FIR-READ-RTN
              END-IF
     END-PERFORM.
 FIR-DELETE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     表示項目を振替依頼ファイルに転送             *
*--------------------------------------------------------------*
 DSP-TO-NYU             SECTION.
     IF    SRKBN    =    1
           MOVE  SPACE            TO   FIR-REC
           INITIALIZE                  FIR-REC
           MOVE   WK-EIGYO        TO   FIR-F01
           MOVE   IRINO           TO   FIR-F02
           MOVE   I               TO   FIR-F03
           MOVE   SYS-DATE2       TO   FIR-F98
           MOVE   SYS-DATE2       TO   FIR-F99
     ELSE
           MOVE   SYS-DATE2       TO   FIR-F99
     END-IF.
* 画面データを振替依頼ファイルの項目にセット
     MOVE   WK-IRI-DATE           TO   FIR-F04.
     MOVE   BASY01                TO   FIR-F05.
     MOVE   BASY02                TO   FIR-F06.
     MOVE   SYOCD (I)             TO   FIR-F07.
     MOVE   HIN1(I)               TO   FIR-F08(1:5).
     MOVE   HIN2(I)               TO   FIR-F08(6:2).
     MOVE   HIN3(I)               TO   FIR-F08(8:1).
     MOVE   SURYOU(I)             TO   FIR-F09.
     MOVE   IRIKBN                TO   FIR-F10.
*
 DSP-TO-FIR-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ヘッド部読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 FIR-TO-HEAD            SECTION.
* ヘッド部のデータを画面にセット
     MOVE   FIR-F02          TO   IRINO.
     MOVE   FIR-F04(3:6)     TO   SYUYMD.
     MOVE   FIR-F10          TO   IRIKBN.
     IF     FIR-F10     =    "1"
            MOVE   NC"依頼する"   TO   IRIMEI
     ELSE
            MOVE   SPACE          TO   IRIMEI
     END-IF.
*
     MOVE   FIR-F05          TO   BASY01.
     MOVE   FIR-F06          TO   BASY02.
     IF     FIR-F05     NOT =     SPACE
            MOVE   FIR-F05   TO   SOK-F01
            PERFORM     SOK-READ-RTN
            IF     INV-FLG   =    1
                   MOVE SPACE     TO   BSYM1
            ELSE
                   MOVE SOK-F02   TO   BSYM1
            END-IF
     END-IF.
     IF     FIR-F06     NOT =     SPACE
            MOVE   FIR-F06   TO   SOK-F01
            PERFORM     SOK-READ-RTN
            IF     INV-FLG   =    1
                   MOVE SPACE     TO   BSYM2
            ELSE
                   MOVE SOK-F02   TO   BSYM2
            END-IF
     END-IF.
 FIR-TO-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ボディ部読込　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 FIR-TO-BODY            SECTION.
* ボディ部データセット
     MOVE     FIR-F07             TO   SYOCD (I).
     MOVE     FIR-F08(1:5)        TO   HIN1  (I).
     MOVE     FIR-F08(6:2)        TO   HIN2  (I).
     MOVE     FIR-F08(8:1)        TO   HIN3  (I).
     MOVE     FIR-F09             TO   SURYOU(I).
* 商品名称取得
     MOVE     FIR-F07             TO   MEI-F011.
     MOVE     FIR-F08(1:5)        TO   MEI-F0121.
     MOVE     FIR-F08(6:2)        TO   MEI-F0122.
     MOVE     FIR-F08(8:1)        TO   MEI-F0123.
     PERFORM  MEI-READ-RTN.
     IF       INV-FLG        =    1
              MOVE      12        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR  OF SYOCD(I)
              MOVE     "R"        TO   EDIT-OPTION  OF SYOCD(I)
              MOVE     "R"        TO   EDIT-OPTION  OF HIN1(I)
              MOVE     "R"        TO   EDIT-OPTION  OF HIN2(I)
              MOVE     "R"        TO   EDIT-OPTION  OF HIN3(I)
     ELSE
              MOVE      MEI-F021       TO   WK-HINMEI1
              MOVE      MEI-F022       TO   WK-HINMEI2
              MOVE      WK-HINMEI      TO   HINMEI(I)
     END-IF.
 FIR-TO-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 DSP-RD-RTN           SECTION.
*
     IF       ERR-FLG  =  0
              MOVE      SPACE               TO   MESAGE
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MESAGE
     END-IF.
     MOVE     "ALLF"         TO   DSP-GRP.
     PERFORM  DSP-WRITE-RTN.
*
     IF       ERR-FLG  NOT  =  0
              MOVE      "AL"           TO   DSP-PRO
              MOVE      0              TO   ERR-FLG
     ELSE
              MOVE      "NE"           TO   DSP-PRO
     END-IF.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 DSP-RD-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 DSP-WRITE-RTN          SECTION.
     MOVE     HEN-DATE       TO   SYSYMD.
     MOVE     HEN-TIME       TO   SYSTIM.
     MOVE     WK-EIGYO       TO   EIGYO.
     MOVE     WK-EIGMEI      TO   EIGMEI.
*
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FFU00101.
 DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   振替依頼ファイル ＲＥＡＤ
*--------------------------------------------------------------*
 FIR-READ-RTN      SECTION.
     MOVE     ZERO      TO    FIR-FLG.
     READ     FUIRAIF   NEXT  AT  END
              MOVE      1         TO   FIR-FLG
              GO   TO   FIR-READ-EXIT
     END-READ.
* 伝票_ブレイク
     IF  IRINO     NOT =     FIR-F02
         MOVE      1    TO   FIR-FLG
         GO        TO   FIR-READ-EXIT
     END-IF.
* ワークステイションが本社以外の時，自倉庫以外読み飛ばし
     IF  WK-SOKCD      =  "01"  AND
         WK-DSOKCD     =  "01"
         IF   WK-EIGYO    NOT =   FIR-F01
              MOVE     FIR-F01    TO   EIGYO  WK-EIGYO
              MOVE     FIR-F01    TO   SOK-F01
              PERFORM     SOK-READ-RTN
              IF       INV-FLG    =    1
                   MOVE      SPACE     TO   EIGMEI  WK-EIGMEI
              ELSE
                   MOVE      SOK-F02   TO   EIGMEI  WK-EIGMEI
              END-IF
         END-IF
     ELSE
         IF   WK-EIGYO    NOT =   FIR-F01
              GO  TO  FIR-READ-RTN
         END-IF
     END-IF.
*
     IF   SRKBN        =   9
          REWRITE  FIR-REC
     END-IF.
 FIR-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   振替依頼ファイル ＲＥＡＤ１
*--------------------------------------------------------------*
 FIR-READ1-RTN       SECTION.
     READ     FUIRAIF   NEXT  AT  END
              MOVE      1         TO   FIR-FLG
              GO   TO   FIR-READ1-EXIT
     END-READ.
* ワークステイションが本社以外の時，自倉庫以外読み飛ばし
     IF  WK-SOKCD      =  "01"  AND
         WK-DSOKCD     =  "01"
         IF   WK-EIGYO    NOT =   FIR-F01
              MOVE     FIR-F01    TO   EIGYO  WK-EIGYO
              MOVE     FIR-F01    TO   SOK-F01
              PERFORM     SOK-READ-RTN
              IF       INV-FLG    =    1
                   MOVE      SPACE     TO   EIGMEI  WK-EIGMEI
              ELSE
                   MOVE      SOK-F02   TO   EIGMEI  WK-EIGMEI
              END-IF
         END-IF
     ELSE
         IF   WK-EIGYO    NOT =   FIR-F01
              GO  TO  FIR-READ1-RTN
         END-IF
     END-IF.
*
     IF   SRKBN        =   9
          REWRITE  FIR-REC
     END-IF.
 FIR-READ1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   条件ファイル　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 JYO-RD-RTN         SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 JYO-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   商品名称マスタ　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 MEI-READ-RTN       SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HMEIMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  ALL   倉庫マスタ　　　　ＲＥＡＤ　　　　　　　　　 *
*--------------------------------------------------------------*
 SOK-READ-RTN         SECTION.
     MOVE     0         TO   INV-FLG.
     READ     ZSOKMS    INVALID   KEY
              MOVE      1         TO   INV-FLG
     END-READ.
 SOK-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF SRKBN
                                  EDIT-CURSOR OF EIGYO
                                  EDIT-CURSOR OF IRINO
                                  EDIT-CURSOR OF SYUYMD
                                  EDIT-CURSOR OF IRIKBN
                                  EDIT-CURSOR OF BASY01
                                  EDIT-CURSOR OF BASY02.
     MOVE     "M"            TO   EDIT-OPTION OF SRKBN
                                  EDIT-OPTION OF EIGYO
                                  EDIT-OPTION OF IRINO
                                  EDIT-OPTION OF SYUYMD
                                  EDIT-OPTION OF IRIKBN
                                  EDIT-OPTION OF BASY01
                                  EDIT-OPTION OF BASY02.
*
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY-RTN           SECTION.
     PERFORM  VARYING  I  FROM  1  BY  1  UNTIL  I  >  6
       MOVE     " "      TO   EDIT-CURSOR OF SYOCD (I)
                              EDIT-CURSOR OF HIN1(I)
                              EDIT-CURSOR OF HIN2(I)
                              EDIT-CURSOR OF HIN3(I)
                              EDIT-CURSOR OF SURYOU(I)
       MOVE     "M"      TO   EDIT-OPTION OF SYOCD (I)
                              EDIT-OPTION OF HIN1(I)
                              EDIT-OPTION OF HIN2(I)
                              EDIT-OPTION OF HIN3(I)
                              EDIT-OPTION OF SURYOU(I)
     END-PERFORM.
 CLR-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KAKNIN.
     MOVE     "M"        TO   EDIT-OPTION OF KAKNIN.
 CLR-TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      プロテクトセット（明細行）　　　　　　　　　*
*--------------------------------------------------------------*
 PRO-BODY-RTN           SECTION.
*未入力行にプロテクトセット
     MOVE  "X"           TO   EDIT-STATUS  OF  SYOCD (I)
                              EDIT-STATUS  OF  SYOCD (I)
                              EDIT-STATUS  OF  HIN1(I)
                              EDIT-STATUS  OF  HIN2(I)
                              EDIT-STATUS  OF  HIN3(I)
                              EDIT-STATUS  OF  SURYOU(I)
                              EDIT-STATUS  OF  HINMEI(I).
 PRO-BODY-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
