# NKE0460L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0460L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　新_卸原票出力　　　　　　　　　　*
*    作成日／作成者　　　：　2019/02/24 INOUE                  *
*    処理概要　　　　　　：　指定された倉庫コードの_卸原票を　*
*                            出力する　　　　　　　　　　　　　*
*    変更履歴　　　　　　：　　　　　　　　　　　　　　　　　　*
*    　更新日／更新者　　：　　　　　　　　　　　　　　　　　　*
*                            　　　　　　　　　　　　          *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            NKE0460L.
*流用                   STA0021L.TOKSLIB
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K6500.
 OBJECT-COMPUTER.       FACOM-K6500.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS
         YA        IS   YA
         YB        IS   YB.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  棚卸確定累積データ  >>---*
     SELECT   RUITANF   ASSIGN    TO        DA-01-VI-RUITANL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   RUI-F03 RUI-F01
                        FILE      STATUS    IS   RUI-STATUS.
*
*---<<  倉庫マスタ     >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
*---<<  担当者変換マスタ     >>---*
     SELECT   TANHENF   ASSIGN    TO        DA-01-VI-TANHENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   HEN-F01
                        FILE      STATUS    IS   HEN-STATUS.
*
*---<<  担当者マスタ     >>---*
     SELECT   HTANMS    ASSIGN    TO        DA-01-VI-TANMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TAN-F01 TAN-F02
                        FILE      STATUS    IS   TAN-STATUS.
*
*---<<  商品名称マスタ >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<<  条件ファイル　 >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
*---<<  _卸原票　　　 >>---*
     SELECT   PRINTF    ASSIGN    TO        LP-04.
*
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
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---((  _卸確定累積データ  ))---*
 FD  RUITANF.
     COPY     RUITANF   OF        XFDLIB
              JOINING   RUI       PREFIX.
*---((  倉庫マスタ    ))---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*---((  商品名称マスタ))---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---((  担当者変換マスタ    ))---*
 FD  TANHENF.
     COPY     TANHENF   OF        XFDLIB
              JOINING   HEN       PREFIX.
*---((  担当者マスタ    ))---*
 FD  HTANMS.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*---((  条件ファイル　))---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---((  _卸原票　　　))---*
 FD  PRINTF
     LINAGE   IS        60        LINES.
 01  PRINT-REC               PIC  X(200).
*---((  画面ファイル  ))---*
 FD  DSPF.
     COPY     FKE04601  OF        XMDLIB.
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
     02  RUI-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  TAN-STATUS          PIC  X(02).
     02  HEN-STATUS          PIC  X(02).
****  フラグ      ****
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  FLG-NUM             PIC  X(03)  VALUE SPACE.
****　カウンタ    ****
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE ZERO.
     02  PAGE-CNT            PIC  9(07)  VALUE ZERO.
     02  IX                  PIC  9(01)  VALUE ZERO.
**** 日付／時刻   ****
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
**** 画面表示日付編集 ****
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
**** 画面表示時刻編集 ****
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
**** 部門情報退避 ****
 01  WK-BUMON-SAV.
     03  WK-BUMON-CD              PIC  9(09)V9(02).
     03  WK-BUMON-CDR             REDEFINES   WK-BUMON-CD.
         05  WK-BUMON-CD1         PIC  9(05).
         05  WK-BUMON-CD2         PIC  9(04).
         05  WK-BUMON-CD3         PIC  9(02).
     03  WK-BUMON-NAME            PIC  N(20)  VALUE  SPACE.
****　ワーク      ****
 01  WORK-AREA.
     02  WK-TANAOROSINO.
       03  WK-GENPYONO       PIC  X(06)  VALUE SPACE.
       03  WK-GYONO          PIC  X(01)  VALUE SPACE.
     02  BRK-GENPYONO        PIC  X(06)  VALUE SPACE.
     02  WK-SYOHINCD         PIC  X(08)  VALUE SPACE.
     02  WK-HINTANCD         PIC  X(08)  VALUE LOW-VALUE.
     02  WK-HINTANCD2        PIC  X(02)  VALUE LOW-VALUE.
     02  WK-TANABAN          PIC  X(04)  VALUE SPACE.
 01  WK-JYO-SOKCD            PIC  X(02).
 01  WK-DAISOKCD             PIC  X(02).
 01  WK-RUI-F08              PIC  9(08)  VALUE ZERO.
****　サブルーチン用連絡領域（日本語変換の為）****
 01  WK-RTNCD                PIC S9(09)  BINARY.
 01  WK-INLT                 PIC S9(04)  BINARY  VALUE  8.
 01  WK-INDT                 PIC  X(08).
 01  WK-OTLT                 PIC S9(04)  BINARY  VALUE 16.
 01  WK-OTDT                 PIC  N(08).
 01  WK-DTLT                 PIC S9(04)  BINARY.
****　帳票        ****
*---  見出し１    ---*
 01  HEAD01.
*****02  FILLER              PIC  X(02)  VALUE SPACE.
     02  FILLER              PIC  X(69)  VALUE SPACE.
     02  HEAD01-YY           PIC  9(04).
     02  FILLER              PIC  X(01)  VALUE ".".
     02  HEAD01-MM           PIC  Z9.
     02  FILLER              PIC  X(01)  VALUE ".".
     02  HEAD01-DD           PIC  Z9.
*---  見出し２    ---*
 01  HEAD021             CHARACTER    TYPE   IS     YA.
*****02  FILLER              PIC  X(02)  VALUE SPACE.
     02  FILLER              PIC  X(05)  VALUE SPACE.
     02  HEAD021-TANANO      PIC  N(06).
*↓
*    02  FILLER              PIC  X(56)  VALUE SPACE.
     02  FILLER              PIC  X(37)  VALUE SPACE.
     02  HEAD021-TANNM1      PIC  N(02)  VALUE SPACE.
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  HEAD021-TANNM2      PIC  N(02)  VALUE SPACE.
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  HEAD021-TANNM3      PIC  N(02)  VALUE SPACE.
     02  FILLER              PIC  X(05)  VALUE SPACE.
*↑
     02  HEAD021-PAGE        PIC  ZZZ9.
*
 01  HEAD022             CHARACTER    TYPE   IS     YA.
     02  FILLER              PIC  X(54)  VALUE SPACE.
     02  FILLER              PIC  X(37)  VALUE SPACE.
     02  HEAD022-TANNM1      PIC  N(02)  VALUE SPACE.
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  HEAD022-TANNM2      PIC  N(02)  VALUE SPACE.
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  HEAD022-TANNM3      PIC  N(02)  VALUE SPACE.
*
*---  見出し３    ---*
 01  HEAD03              CHARACTER    TYPE   IS     YA.
     02  FILLER              PIC  X(04)  VALUE SPACE.
     02  HEAD03-BUMONCD      PIC  N(04)  VALUE SPACE.
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  HEAD03-BUMONMEI     PIC  N(06)  VALUE SPACE.
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  HEAD03-BASYO        PIC  N(02).
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  HEAD03-BASYOMEI     PIC  N(06).
     02  FILLER              PIC  X(03)  VALUE SPACE.
     02  HEAD03-TANABAN      PIC  N(04)  VALUE SPACE.
     02  FILLER              PIC  X(16)  VALUE SPACE.
     02  HEAD03-TANNM        PIC  N(05)  VALUE SPACE.
*
*---  明細１　    ---*
 01  BODY01              CHARACTER    TYPE   IS     YA.
     02  FILLER              PIC  X(02)  VALUE SPACE.
     02  BODY01-SYOHINCD     PIC  N(08).
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  BODY01-HINTANCD     PIC  N(08).
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  BODY01-STOKNO       PIC  N(04)  VALUE SPACE.
     02  FILLER              PIC  X(05)  VALUE SPACE.
     02  BODY01-TANA2        PIC  N(02)  VALUE SPACE.
     02  FILLER              PIC  X(01)  VALUE SPACE.
     02  BODY01-TANASU       PIC  N(08)  VALUE SPACE.
*
*---  明細２　    ---*
 01  BODY02              CHARACTER    TYPE  IS     YA.
*****02  FILLER              PIC  X(02)  VALUE SPACE.
     02  FILLER              PIC  X(06)  VALUE SPACE.
     02  BODY02-SYOHINMEI    PIC  N(15).
     02  FILLER              PIC  X(16)  VALUE SPACE.
     02  BODY02-NENDO        PIC  N(02)  VALUE SPACE.
*---  明細３　    ---*
 01  BODY03.
     02  FILLER              PIC  X(06)  VALUE SPACE.
     02  BODY03-1        CHARACTER    TYPE  IS     YA.
       03  BODY03-SYOHINMEI  PIC  N(15).
     02  FILLER              PIC  X(09)  VALUE SPACE.
     02  BODY03-2        CHARACTER    TYPE  IS     YB.
       03  BODY03-TANI       PIC  N(02).
     02  FILLER              PIC  X(20)  VALUE SPACE.
     02  BODY03-TANA         PIC  X(08).
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01              PIC N(20) VALUE
         NC"５：終了".
     02  PMSG02              PIC N(20) VALUE
         NC"４：取消　５：終了　６：項目戻し".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "NKE0460L".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-CD              PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ        ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(20)  VALUE
            NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"倉庫コードを入力して下さい。".
     02  MSG-ERR3            PIC  N(20)  VALUE
            NC"倉庫コードに誤りがあります。".
     02  MSG-ERR4            PIC  N(20)  VALUE
            NC"指定の倉庫コードは，扱っていません。".
     02  MSG-ERR5            PIC  N(20)  VALUE
            NC"確認は，Ｙで入力して下さい。".
 01  ERR-MSG-ALL             REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS  5   TIMES.
****  エラーメッセージ ＦＯＲ　ファイル  ***
 01  FILE-ERROR.
     02  FILE-ERR1           PIC  N(10)  VALUE
            NC"_卸ファイル　異常！".
     02  FILE-ERR2           PIC  N(10)  VALUE
            NC"倉庫マスタ　　異常！".
     02  FILE-ERR3           PIC  N(10)  VALUE
            NC"商品名称マスタ異常！".
     02  FILE-ERR4           PIC  N(10)  VALUE
            NC"条件ファイル　異常！".
     02  FILE-ERR5           PIC  N(10)  VALUE
            NC"画面ファイル　異常！".
     02  FILE-ERR6           PIC  N(10)  VALUE
            NC"担当者マスタ　異常！".
*
**** 日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
****  ＬＩＮＫ領域  ***
 LINKAGE                     SECTION.
 01  LINK-IN-BUMCD       PIC  X(04).
 01  LINK-IN-TANCD       PIC  X(02).
 01  LINK-IN-WSID        PIC  X(08).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION   USING   LINK-IN-BUMCD
                                                LINK-IN-TANCD
                                                LINK-IN-WSID.
**
 DECLARATIVES.
**_卸確定累積データ
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    RUITANF.
     MOVE     "RUITANF"          TO   ERR-FL-ID.
     MOVE     RUI-STATUS         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1       UPON   CONS.
     DISPLAY  MSG-ABEND2       UPON   CONS.
     MOVE     4000               TO   PROGRAM-STATUS.
     STOP     RUN.
**倉庫ファイル
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZSOKMS.
     MOVE     "ZSOKMS"           TO   ERR-FL-ID.
     MOVE     SOK-STATUS         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1       UPON   CONS.
     DISPLAY  MSG-ABEND2       UPON   CONS.
     MOVE     4000               TO   PROGRAM-STATUS.
     STOP     RUN.
**商品名称マスタ
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "HMEIMS"           TO   ERR-FL-ID.
     MOVE     MEI-STATUS         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1       UPON   CONS.
     DISPLAY  MSG-ABEND2       UPON   CONS.
     MOVE     4000               TO   PROGRAM-STATUS.
     STOP     RUN.
**条件ファイル
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"          TO   ERR-FL-ID.
     MOVE     JYO-STATUS         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1       UPON   CONS.
     DISPLAY  MSG-ABEND2       UPON   CONS.
     MOVE     4000               TO   PROGRAM-STATUS.
     STOP     RUN.
**画面ファイル
 FILEERR-SEC5                SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      DSPF.
     MOVE     "DSPF    "         TO   ERR-FL-ID.
     MOVE     DSP-STATUS         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1       UPON   CONS.
     DISPLAY  MSG-ABEND2       UPON   CONS.
     MOVE     4000               TO   PROGRAM-STATUS.
     STOP     RUN.
**担当者マスタ
 FILEERR-SEC6                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HTANMS.
     MOVE     "HTANMS"           TO   ERR-FL-ID.
     MOVE     TAN-STATUS         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1       UPON   CONS.
     DISPLAY  MSG-ABEND2       UPON   CONS.
     MOVE     4000               TO   PROGRAM-STATUS.
     STOP     RUN.
**担当者変換マスタ
 FILEERR-SEC7                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    TANHENF.
     MOVE     "TANHENF"          TO   ERR-FL-ID.
     MOVE     HEN-STATUS         TO   ERR-STCD.
     DISPLAY  MSG-ABEND1       UPON   CONS.
     DISPLAY  MSG-ABEND2       UPON   CONS.
     MOVE     4000               TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 FKE04601-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 FKE04601-END.
     EXIT.
****************************************************************
*      1.0　　初期処理
****************************************************************
 INIT-SEC                    SECTION.
     OPEN    I-O             DSPF.
     OPEN    INPUT           RUITANF.
     OPEN    INPUT           ZSOKMS.
     OPEN    INPUT           HMEIMS.
     OPEN    INPUT           HJYOKEN.
     OPEN    INPUT           HTANMS.
     OPEN    INPUT           TANHENF.
     OPEN    OUTPUT          PRINTF.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY HEAD01-YY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM   HEAD01-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD   HEAD01-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
*条件Ｆ索引（倉庫コード取得）
     MOVE    65                  TO   JYO-F01.
     MOVE    LINK-IN-WSID               TO   JYO-F02.
     READ    HJYOKEN
           INVALID
             DISPLAY  FILE-ERR4               UPON CONS
             DISPLAY "ERR INVALID KEY1= " JYO-F01 UPON CONS
             DISPLAY "ERR INVALID KEY2= " JYO-F02 UPON CONS
             PERFORM    END-SEC
             STOP RUN
     END-READ.
     MOVE    JYO-F14(1:2)        TO   WK-JYO-SOKCD.
     MOVE    JYO-F15(1:2)        TO   WK-DAISOKCD.
*条件Ｆ索引（部門コード取得）
     MOVE    99                  TO   JYO-F01.
     MOVE    "BUMON"             TO   JYO-F02.
     READ    HJYOKEN
           INVALID
             DISPLAY  FILE-ERR4               UPON CONS
             DISPLAY "ERR INVALID KEY1= " JYO-F01 UPON CONS
             DISPLAY "ERR INVALID KEY2= " JYO-F02 UPON CONS
             PERFORM  END-SEC
             STOP RUN
     END-READ.
     MOVE    JYO-F05             TO   WK-BUMON-CD.
     MOVE    JYO-F03             TO   WK-BUMON-NAME.
*
     PERFORM    DSP-INIT-SEC.
*
     PERFORM    WRITE-SELECT-SEC.
     IF   END-FLG   NOT =   "END"
         PERFORM    RUITANF-READ-SEC
*↓？                 ↓原票データの「棚番６」
*        MOVE       RUI-F04(1:4) TO   WK-TANABAN
*↑？
     END-IF.
 INIT-END.
     EXIT.
****************************************************************
*      1.1　　出力条件指定　処理
****************************************************************
 WRITE-SELECT-SEC            SECTION.
*----- 初画面出力 ------*
     MOVE    PMSG01              TO   PFKGID.
     PERFORM    DSP-WRITE-SEC.
*----- 画面ＲＥＡＤ ----*
 WS010.
     MOVE    "BODY"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　■１ ----*
     MOVE    "M"                 TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SOKCD.
     EVALUATE      DSP-FUNC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             GO                  TO   WRITE-SELECT-END
         WHEN     "E000"
             CONTINUE
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS010
     END-EVALUATE.
*----- 入力項目チェック---*
*（条件ファイルでの存在チェック・本社の時は全倉庫可）
     MOVE    SPACE          TO   SOKMEI.
     IF   WK-DAISOKCD  NOT =  "01"
          IF   SOKCD   NOT =  WK-JYO-SOKCD
               IF   ERR-CD   =   ZERO
                    MOVE    4           TO   ERR-CD
               END-IF
               GO                TO   SELECT-01
          END-IF
     END-IF.
     IF   SOKCD   =   SPACE
          MOVE  NC"全倉庫"       TO   SOKMEI
     ELSE
*（存在チェック）*
          MOVE    SOKCD          TO   SOK-F01
          READ    ZSOKMS
              INVALID   KEY
                  IF   ERR-CD   =   ZERO
                      MOVE    3       TO   ERR-CD
                  END-IF
              NOT   INVALID   KEY
                  MOVE    SOK-F02     TO   SOKMEI
          END-READ
     END-IF.
*
 SELECT-01.
*----- エラーが有った時-----*
     IF   ERR-CD  NOT =   ZERO
         MOVE    "R"             TO   EDIT-OPTION   OF   SOKCD
         MOVE    "C"             TO   EDIT-CURSOR   OF   SOKCD
         PERFORM    DSP-ERR-SEC
         GO                      TO   WS010
     END-IF.
*----- ＢＯＤＹ部の出力-----*
     MOVE    PMSG02              TO   PFKGID.
     MOVE    "Y"                 TO   KAKNIN.
     MOVE    "M"                 TO   EDIT-OPTION   OF   SOKCD.
     MOVE    " "                 TO   EDIT-CURSOR   OF   SOKCD.
     MOVE    "C"                 TO   EDIT-CURSOR   OF   KAKNIN.
     MOVE    SPACE               TO   ERRMSG.
     PERFORM    DSP-WRITE-SEC.
*----- 確認画面の入力-----*
 WS020.
     MOVE    "TAIL"              TO   DSP-GROUP.
     PERFORM    DSP-READ-SEC.
*----- ＰＦキー判定　■２ ----*
     EVALUATE      DSP-FUNC
         WHEN     "F004"
             PERFORM    DSP-INIT-SEC
             GO                  TO   WRITE-SELECT-SEC
         WHEN     "F005"
             MOVE    "END"       TO   END-FLG
             GO                  TO   WRITE-SELECT-END
         WHEN     "F006"
             GO                  TO   WRITE-SELECT-SEC
         WHEN     "E000"
             CONTINUE
         WHEN      OTHER
             MOVE    1           TO   ERR-CD
             MOVE    "TAIL"      TO   DSP-GROUP
             PERFORM    DSP-ERR-SEC
             GO                  TO   WS020
     END-EVALUATE.
     IF   KAKNIN   NOT =   "Y"   AND
          ERR-CD       =   ZERO
          MOVE    5              TO   ERR-CD
          MOVE    "TAIL"         TO   DSP-GROUP
          PERFORM    DSP-ERR-SEC
          GO                     TO   WS020
     END-IF.
 WRITE-SELECT-END.
     EXIT.
****************************************************************
*      1.1.1　初期画面　出力　処理
****************************************************************
 DSP-INIT-SEC           SECTION.
     MOVE    SPACE               TO   FKE04601.
     MOVE    HEN-DATE            TO   SDATE.
     MOVE    HEN-TIME            TO   STIME.
     IF   WK-DAISOKCD     NOT =  "01"
          MOVE    WK-JYO-SOKCD        TO   SOK-F01   SOKCD
          READ    ZSOKMS
              INVALID KEY
                  CONTINUE
              NOT INVALID KEY
                  MOVE    SOK-F02     TO   SOKMEI
          END-READ
     END-IF.
*
     MOVE    SPACE               TO   DSP-CONTROL.
     MOVE    "CL"                TO   DSP-PROC.
     MOVE    "FKE04601"          TO   DSP-FORMAT.
     MOVE    "ALL"               TO   DSP-GROUP.
     MOVE    PMSG01              TO   PFKGID.
     WRITE                  FKE04601.
 DSP-INIT-END.
     EXIT.
****************************************************************
*      1.1.2　画面ＲＥＡＤ　処理
****************************************************************
 DSP-READ-SEC           SECTION.
     MOVE    "NE"                TO   DSP-PROC.
     READ    DSPF
         AT   END
             GO                  TO   DSP-READ-END
     END-READ.
     IF   DSP-STATUS   NOT =   ZERO
         DISPLAY   FILE-ERR5   UPON   CONS
     END-IF.
 DSP-READ-END.
     EXIT.
****************************************************************
*      1.1.3 　画面ＷＲＩＴＥ　処理
****************************************************************
 DSP-WRITE-SEC          SECTION.
******
     MOVE    "ALL"               TO   DSP-GROUP.
******
     MOVE    SPACE               TO   DSP-PROC.
     WRITE                 FKE04601.
 DSP-WRITE-END.
     EXIT.
****************************************************************
*      1.1.4 　エラーメッセージセット　処理
****************************************************************
 DSP-ERR-SEC                 SECTION.
     MOVE    ERR-MSG(ERR-CD)     TO   ERRMSG.
     MOVE    ZERO                TO   ERR-CD.
     PERFORM    DSP-WRITE-SEC.
 DSP-ERR-END.
     EXIT.
****************************************************************
*      1.2 　　_卸ファイル　読込　処理
****************************************************************
 RUITANF-READ-SEC            SECTION.
     READ    RUITANF
         AT   END
           MOVE    "END"         TO   END-FLG
           GO                    TO   RUITANF-READ-END
     END-READ.
     IF   SOKCD     NOT =   SPACE
          IF   RUI-F03   NOT =   SOKCD
               GO                     TO   RUITANF-READ-SEC
          END-IF
     END-IF.
     ADD   1                     TO   READ-CNT.
     MOVE  RUI-F01               TO   WK-TANAOROSINO.
 RUITANF-READ-END.
     EXIT.
****************************************************************
*      2.0 　　メイン処理
****************************************************************
 MAIN-SEC                    SECTION.
     IF  RUI-F01(1:6)  NOT =   BRK-GENPYONO
         PERFORM HEAD-WRITE-SEC
         MOVE    RUI-F01(1:6)    TO   BRK-GENPYONO
*↓？                 ↓原票データの「棚番６」
         MOVE    RUI-F04(1:4)    TO   WK-TANABAN
*↑？                 ↓原票データの「棚番６」
     END-IF.
*
     PERFORM    BODY-WRITE-SEC.
*
     PERFORM    RUITANF-READ-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      2.1 　　見出し　出力　処理
****************************************************************
 HEAD-WRITE-SEC              SECTION.
     IF  PAGE-CNT NOT =   ZERO
         MOVE     SPACE          TO   PRINT-REC
         WRITE    PRINT-REC   AFTER   PAGE
     END-IF.
*----見出し２　編集       ----*
*    棚卸_
     MOVE    RUI-F01             TO   WK-INDT.
     PERFORM   ZENKAKU-HENKAN-SEC.
     MOVE    WK-OTDT             TO   HEAD021-TANANO.
*    担当者１
     MOVE    RUI-F10             TO   HEN-F01.
     READ    TANHENF
         INVALID   KEY
             MOVE  NC"＊＊"      TO   HEAD021-TANNM1
             GO                  TO   HEAD-WRITE-01
     END-READ.
     MOVE    LINK-IN-BUMCD       TO   TAN-F01.
     MOVE    HEN-F03             TO   TAN-F02.
     READ    HTANMS
         INVALID   KEY
             MOVE  NC"＊＊"
                                 TO   HEAD021-TANNM1
*                                     HEAD022-TANNM1
         NOT   INVALID   KEY
             MOVE  TAN-F03(1:2)  TO   HEAD021-TANNM1
*            MOVE  TAN-F03(3:2)  TO   HEAD022-TANNM1
     END-READ.
 HEAD-WRITE-01.
*    担当者２
     MOVE    SPACE               TO   HEAD021-TANNM2
*    MOVE    SPACE               TO   HEAD022-TANNM2
*    担当者３
     MOVE    RUI-F11             TO   HEN-F01.
     READ    TANHENF
         INVALID   KEY
             MOVE  NC"＊＊"      TO   HEAD021-TANNM3
             GO                  TO   HEAD-WRITE-02
     END-READ.
     MOVE    LINK-IN-BUMCD       TO   TAN-F01.
     MOVE    HEN-F03             TO   TAN-F02.
     READ    HTANMS
         INVALID   KEY
             MOVE  NC"＊＊"
                                 TO   HEAD021-TANNM3
*                                     HEAD022-TANNM3
         NOT   INVALID   KEY
             MOVE  TAN-F03(1:2)  TO   HEAD021-TANNM3
*            MOVE  TAN-F03(3:2)  TO   HEAD022-TANNM3
     END-READ.
*
 HEAD-WRITE-02.
*----見出し３　編集       ----*
     MOVE    RUI-F03             TO   WK-INDT.
     PERFORM   ZENKAKU-HENKAN-SEC.
     MOVE    WK-OTDT             TO   HEAD03-BASYO.
*    倉庫マスタ索引　倉庫名取得
     MOVE    RUI-F03             TO   SOK-F01.
     READ    ZSOKMS
         INVALID   KEY
             MOVE    NC"＊＊＊＊＊＊"
                                 TO   HEAD03-BASYOMEI
         NOT   INVALID   KEY
             MOVE    SOK-F02     TO   HEAD03-BASYOMEI
     END-READ.
*    棚番
     MOVE    RUI-F04(1:4)        TO   WK-INDT.
     PERFORM   ZENKAKU-HENKAN-SEC.
     MOVE    WK-OTDT             TO   HEAD03-TANABAN
*    部門ＣＤ／名称
     MOVE    RUI-F02             TO   WK-INDT.
     PERFORM   ZENKAKU-HENKAN-SEC.
     MOVE    WK-OTDT             TO   HEAD03-BUMONCD.
     MOVE    WK-BUMON-NAME       TO   HEAD03-BUMONMEI.
     EVALUATE RUI-F02
         WHEN 291     MOVE NC"営業２北海道" TO HEAD03-BUMONMEI
         WHEN 292     MOVE NC"営業２東日本" TO HEAD03-BUMONMEI
         WHEN 293     MOVE NC"営業２西日本" TO HEAD03-BUMONMEI
         WHEN 294     MOVE NC"営業２九州　" TO HEAD03-BUMONMEI
         WHEN OTHER   MOVE NC"営業２東日本" TO HEAD03-BUMONMEI
     END-EVALUATE.
*    担当者（責任者）
     MOVE    RUI-F12              TO   HEN-F01.
     READ    TANHENF
         INVALID   KEY
             MOVE  NC"＊＊＊＊＊" TO   HEAD03-TANNM
             GO                   TO   HEAD-WRITE-03
     END-READ.
     MOVE    LINK-IN-BUMCD        TO   TAN-F01.
     MOVE    HEN-F03              TO   TAN-F02.
     READ    HTANMS
         INVALID   KEY
             MOVE  NC"＊＊＊＊＊"
                                  TO   HEAD03-TANNM
         NOT   INVALID   KEY
             MOVE  TAN-F03        TO   HEAD03-TANNM
     END-READ.
*
*
 HEAD-WRITE-03.
     ADD    1                    TO   PAGE-CNT.
     MOVE   PAGE-CNT             TO   HEAD021-PAGE.
*---- 見出し　出力          ----*
     WRITE    PRINT-REC        FROM   HEAD01   AFTER   1.
     WRITE    PRINT-REC        FROM   HEAD021  AFTER   1.
     WRITE    PRINT-REC        FROM   HEAD022  AFTER   1.
     WRITE    PRINT-REC        FROM   HEAD03   AFTER   1.
     MOVE     SPACE            TO     PRINT-REC.
     WRITE    PRINT-REC                        AFTER   2.
 HEAD-WRITE-END.
     EXIT.
****************************************************************
*      2.1.1 　全角文字変換　処理
****************************************************************
 ZENKAKU-HENKAN-SEC              SECTION.
     CALL   "JCVEBCC"   USING    WK-RTNCD
                                 WK-INLT
                                 WK-INDT
                                 WK-OTLT
                                 WK-OTDT
                                 WK-DTLT.
     IF   WK-RTNCD   NOT =   ZERO
         DISPLAY   "日本語変換エラー！！"  WK-INDT   UPON   CONS
         MOVE    SPACE           TO   WK-OTDT
     END-IF.
*
 ZENKAKU-HENKAN-END.
     EXIT.
****************************************************************
*      2.2 　　明細行　編集＆出力　処理
****************************************************************
 BODY-WRITE-SEC              SECTION.
*---- 明細行１　編集　出力 ----*
*（商品コード全角変換）*
     IF  RUI-F06(1:8)    NOT =   WK-SYOHINCD
         MOVE    RUI-F06(1:8)    TO   WK-INDT
         PERFORM   ZENKAKU-HENKAN-SEC
         MOVE    WK-OTDT         TO   BODY01-SYOHINCD
     END-IF.
*（品単コード全角変換）*
     IF   RUI-F06(9:8)   NOT =   WK-HINTANCD
         MOVE    RUI-F06(9:8)    TO   WK-INDT
         PERFORM   ZENKAKU-HENKAN-SEC
         MOVE    WK-OTDT         TO   BODY01-HINTANCD
     END-IF.
*
*_番セット　　　
     MOVE    SPACE          TO   WK-INDT.
     MOVE    RUI-F04(5:2)   TO   WK-INDT.
     PERFORM   ZENKAKU-HENKAN-SEC.
     MOVE    WK-OTDT        TO   BODY01-TANA2.
*棚卸数量セット　
     MOVE    SPACE          TO   WK-INDT.
     MOVE    RUI-F08        TO   WK-RUI-F08.
     MOVE    WK-RUI-F08     TO   WK-INDT.
     PERFORM ZENKAKU-HENKAN-SEC.
     MOVE    WK-OTDT        TO   BODY01-TANASU.
     MOVE    SPACE          TO   FLG-NUM.
     PERFORM VARYING  IX  FROM 1 BY 1
             UNTIL  ( IX  >  8 ) OR ( FLG-NUM = "NUM" )
             IF       BODY01-TANASU(IX:1)  =  NC"０"
                      MOVE  NC"　"   TO   BODY01-TANASU(IX:1)
             ELSE
                      MOVE  "NUM"    TO   FLG-NUM
             END-IF
     END-PERFORM.
*
*---- 明細行２　編集　出力 ----*
*（商品名称マスタ索引　品名上，下取得）*
     IF  RUI-F06(1:8)    NOT =   WK-SYOHINCD   OR
         RUI-F06(9:8)    NOT =   WK-HINTANCD
         MOVE    RUI-F06(1:8)    TO   MEI-F011
         MOVE    RUI-F06(9:8)    TO   MEI-F012
         READ    HMEIMS
             INVALID   KEY
                 MOVE    NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊"
                                 TO   BODY02-SYOHINMEI
                 MOVE    NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊"
                                 TO   BODY03-SYOHINMEI
             NOT   INVALID   KEY
                 MOVE    MEI-F021
                                 TO   BODY02-SYOHINMEI
                 MOVE    MEI-F022
                                 TO   BODY03-SYOHINMEI
         END-READ
         MOVE    RUI-F06(1:8)    TO   WK-SYOHINCD
         MOVE    RUI-F06(9:8)    TO   WK-HINTANCD
     END-IF.
*年度セット
     MOVE    SPACE          TO   WK-INDT.
     MOVE    RUI-F17        TO   WK-INDT.
     PERFORM   ZENKAKU-HENKAN-SEC.
     MOVE    WK-OTDT        TO   BODY02-NENDO.
     WRITE   PRINT-REC      FROM   BODY01   AFTER   2.
     WRITE   PRINT-REC      FROM   BODY02   AFTER   2.
*---- 明細行３　編集　出力 ----*
*（条件ファイル　索引　単位取得）*
     IF  RUI-F06(14:2)   NOT =   WK-HINTANCD2
         MOVE    "70"            TO   JYO-F01
         MOVE    RUI-F06(14:2)   TO   JYO-F02
         READ    HJYOKEN
             INVALID   KEY
                 MOVE    NC"＊＊"
                                 TO   BODY03-TANI
             NOT   INVALID   KEY
                 MOVE    JYO-F03(1:2)
                                 TO   BODY03-TANI
         END-READ
         MOVE    RUI-F06(14:2)   TO   WK-HINTANCD2
     END-IF.
     MOVE     SPACE              TO   BODY03-TANA.
*
     WRITE    PRINT-REC        FROM   BODY03   AFTER   1.
     ADD    1                    TO   WRITE-CNT.
 BODY-WRITE-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE    RUITANF
              ZSOKMS
              HMEIMS
              HJYOKEN
              HTANMS
              TANHENF
              PRINTF
              DSPF.
     DISPLAY   "ﾀﾅｵﾛｼﾌｱｲﾙ       (IN) = "  READ-CNT   UPON   CONS.
     DISPLAY   "ﾀﾅｵﾛｼﾌｱｲﾙ      (OUT) = "  WRITE-CNT  UPON   CONS.
     DISPLAY   "ﾀﾅｵﾛｼｹﾞﾝﾋﾟｳ(ﾍﾟｰｼﾞｽｳ) = "  PAGE-CNT   UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************

```
