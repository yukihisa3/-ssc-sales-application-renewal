# SSY3903L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY3903L.COB`

## ソースコード

```cobol
****************************************************************
*         SYSTEM･･･ホーマック関東オンラインシステム            *
*        PG-NAME･･･件数リスト発行                              *
*          PG-ID･･･SSY3903L                                    *
*                                            DATE. 00.08.08    *
*                                              BY. Y-YOSHIDA   *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY3903L.
 AUTHOR.                Y-YOSHIDA.
 DATE-WRITTEN.          00/08/08.
*REMARKS.
*
******************************************************************
*                                                                *
 ENVIRONMENT            DIVISION.
*                                                                *
******************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   PICH-YA
         YB        IS   PICH-YB
         YB-21     IS   PICH-YA21.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    検収データＦ
     SELECT   CVCSG001           ASSIGN    TO   CVCSG001
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   DEN-STATUS.
*    プリントファイル
     SELECT   PRINTF             ASSIGN    TO        LP-04.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データＦ＞*****
*    FILE = ｹﾝｼｭｳﾃﾞｰﾀﾌｱｲﾙ
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  DEN-REC.
     03  DEN-01                   PIC  X(01).
     03  DEN-02                   PIC  9(01).
     03  DEN-03                   PIC  X(126).
*
*****＜プリント　ファイル＞*****
 FD  PRINTF                       LINAGE    IS   66   LINES.
 01  PRT-REC                      PIC       X(200).
*
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  DEN-STATUS               PIC  X(02).
     03  PRT-STATUS               PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  CNT-READ                 PIC  9(08)     VALUE   ZERO.
     03  CNT-DENPYO               PIC  9(09)     VALUE   ZERO.
     03  CNT-MEISAI               PIC  9(09)     VALUE   ZERO.
***  日付エリア
 01  WK-SYS-DATE.
     03  WK-YY1                   PIC  9(02)     VALUE   ZERO.
     03  WK-YMD.
         05  WK-YY2               PIC  9(02)     VALUE   ZERO.
         05  WK-MM                PIC  9(02)     VALUE   ZERO.
         05  WK-DD                PIC  9(02)     VALUE   ZERO.
 01  WK-DATE                      PIC  9(08)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  AAA                      PIC  X(01)     VALUE   SPACE.
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR030                  PIC  N(11)     VALUE
         NC"伝票データＦ　異常！！".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY3903L".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
***  ﾌﾟﾘﾝﾄ ｴﾘｱ
*
 01  HEAD-00.
     03  FILLER              PIC  X(01)  VALUE   SPACE.
     03  FILLER              PIC  X(08)  VALUE  "SSY3903L".
     03  FILLER              PIC  X(25)  VALUE   SPACE.
     03  FILLER              PIC  N(14)  VALUE
         NC"＊＊＊　ホーマーグリーン殿　"
         CHARACTER  TYPE IS  PICH-YA.
     03  FILLER              PIC  N(14)  VALUE
         NC"受領データ件数リスト　＊＊＊"
         CHARACTER  TYPE IS  PICH-YA.
     03  FILLER              PIC  X(08)  VALUE  SPACE.
     03  FILLER              PIC  N(03)  VALUE  NC"処理日"
         CHARACTER  TYPE IS  PICH-YA.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HD00-YY             PIC  9(04).
     03  FILLER              PIC  X(1)   VALUE  ".".
     03  HD00-MM             PIC  Z9.
     03  FILLER              PIC  X(1)   VALUE  ".".
     03  HD00-DD             PIC  Z9.
     03  FILLER              PIC  X(3)   VALUE  SPACE.
     03  FILLER              PIC  N(01)  VALUE  NC"頁"
         CHARACTER  TYPE IS  PICH-YA.
     03  FILLER              PIC  X(01)  VALUE  ":".
     03  HD00-PAGE           PIC  ZZZ9.
*
 01  HEAD-01  CHARACTER  TYPE IS  PICH-YA21.
     03  FILLER              PIC  X(05)  VALUE   SPACE.
     03  FILLER              PIC  N(40)  VALUE   ALL NC"＊".
     03  FILLER              PIC  X(05)  VALUE   SPACE.
*
 01  HEAD-02  CHARACTER  TYPE IS  PICH-YA21.
     03  FILLER              PIC  X(05)  VALUE   SPACE.
     03  FILLER              PIC  N(01)  VALUE   NC"＊".
     03  FILLER              PIC  N(38)  VALUE   SPACE.
     03  FILLER              PIC  N(01)  VALUE   NC"＊".
*
 01  HEAD-03  CHARACTER  TYPE IS  PICH-YA21.
     03  FILLER              PIC  X(05)  VALUE   SPACE.
     03  FILLER              PIC  N(10)  VALUE
         NC"＊　　　　　　　　　".
     03  FILLER              PIC  N(20)  VALUE
         NC"ホーマグリーン殿　受領データは、　　　　".
     03  FILLER              PIC  N(10)  VALUE
         NC"　　　　　　　　　＊".
*
 01  HEAD-04  CHARACTER  TYPE IS  PICH-YA21.
     03  FILLER              PIC  X(05)  VALUE   SPACE.
     03  FILLER              PIC  N(10)  VALUE
         NC"＊　　　　　　　　　".
     03  FILLER              PIC  N(06)  VALUE
         NC"　　　伝票　".
     03  HD03-DENPYO         PIC  ZZZ,ZZ9.
     03  FILLER              PIC  X(01)  VALUE   SPACE.
     03  FILLER              PIC  N(05)  VALUE
         NC"枚、明細　".
     03  HD03-KENSU          PIC  ZZZ,ZZ9.
     03  FILLER              PIC  X(01)  VALUE   SPACE.
     03  FILLER              PIC  N(01)  VALUE
         NC"件".
     03  FILLER              PIC  X(01)  VALUE   SPACE.
     03  FILLER              PIC  N(05)  VALUE
         NC"です。　　".
     03  FILLER              PIC  X(01)  VALUE   SPACE.
     03  FILLER              PIC  N(07)  VALUE
         NC"　　　　　　＊".
*
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01           PIC  X(01).
     03  WK-DEPB02           PIC  9(01).
     03  WK-DEPB03           PIC  9(07).
     03  WK-DEPB04           PIC  9(06).
     03  WK-DEPB05           PIC  9(06).
     03  WK-DEPB06.
         05  WK-DEPB061      PIC  9(06).
         05  WK-DEPB062      PIC  9(02).
     03  WK-DEPB07.
         05  WK-DEPB071      PIC  X(20).
         05  WK-DEPB072      PIC  X(20).
     03  WK-DEPB08.
         05  WK-DEPB081      PIC  9(04).
         05  WK-DEPB082      PIC  X(02).
     03  WK-DEPB09           PIC  X(15).
     03  WK-DEPB10.
         05  WK-DEPB101      PIC  9(03).
         05  WK-DEPB102      PIC  X(01).
     03  WK-DEPB11           PIC  9(02).
     03  WK-DEPB12           PIC  9(04).
     03  WK-DEPB13           PIC  9(02).
     03  WK-DEPB14           PIC  9(01).
     03  WK-DEPB15           PIC  9(01).
     03  WK-DEPB16           PIC  X(23).
     03  WK-DEPB17           PIC  X(01).

*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01           PIC  X(01).
     03  WK-DEPD02           PIC  9(01).
     03  WK-DEPD03.
         05  WK-DEPD031      PIC  9(07).
         05  WK-DEPD032      PIC  X(01).
     03  WK-DEPD04.
         05  WK-DEPD041      PIC  X(20).
         05  WK-DEPD042      PIC  X(20).
     03  WK-DEPD05           PIC  9(05)V9.
     03  WK-DEPD06           PIC  9(05)V9.
     03  WK-DEPD07           PIC  9(06)V99.
     03  WK-DEPD08           PIC  9(08).
     03  WK-DEPD09           PIC  9(06).
     03  WK-DEPD10           PIC  9(08).
     03  WK-DEPD11           PIC  X(13).
     03  WK-DEPD12           PIC  X(23).
*    合計レコード退避ワーク
 01  WK-DEPT-REC.
     03  WK-DEPT01           PIC  X(01).
     03  WK-DEPT02           PIC  9(01).
     03  WK-DEPT03           PIC  9(08).
     03  WK-DEPT04           PIC  9(08).
     03  WK-DEPT05           PIC  X(110).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << ﾌﾟﾘﾝﾀ- ｴﾗ- >> ---*
 000-PRINTF-ERR         SECTION.
     USE       AFTER    EXCEPTION   PROCEDURE    PRINTF.
     MOVE     "CVCSG001"       TO   ERR-FL-ID.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << ｼｲﾚ ﾃﾞ-ﾀ ｴﾗ- >> ---*
 000-MAST-ERR           SECTION.
     USE       AFTER    EXCEPTION   PROCEDURE    CVCSG001.
     MOVE     "CVCSG001"       TO   ERR-FL-ID.
     MOVE      DEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR030   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC.
     PERFORM            END-SEC.
*
 CONTROL-END.
     STOP     RUN.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     CVCSG001.
     OPEN     OUTPUT    PRINTF.
*    ワーク初期化
     MOVE     ZERO               TO   CNT-DENPYO.
     MOVE     ZERO               TO   CNT-READ.
*    システム日付取得
     ACCEPT   WK-YMD    FROM     DATE.
     MOVE     "3"                TO   LINK-IN-KBN.
     MOVE     WK-YMD             TO   LINK-IN-YMD6.
     MOVE     ZERO               TO   LINK-IN-YMD8.
     MOVE     ZERO               TO   LINK-OUT-RET.
     MOVE     ZERO               TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"      USING   LINK-IN-KBN
                                      LINK-IN-YMD6
                                      LINK-IN-YMD8
                                      LINK-OUT-RET
                                      LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD       TO   WK-DATE.
*
*    伝票枚数カウント
 INIT001.
     READ     CVCSG001  AT   END
              GO        TO        INIT-EXIT
     END-READ.
     ADD      1         TO   CNT-READ.
*    受領伝票ヘッダーカウント
     IF       DEN-01    =   "H"
              ADD       1    TO   CNT-DENPYO
     END-IF.
*    受領伝票明細カウント
     IF       DEN-01    =   "L"
              ADD       1    TO   CNT-MEISAI
     END-IF.
     GO       TO        INIT001.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*
*    日付セット
     MOVE     WK-DATE(1:4)       TO   HD00-YY.
     MOVE     WK-DATE(5:2)       TO   HD00-MM.
     MOVE     WK-DATE(7:2)       TO   HD00-DD.
     MOVE     1                  TO   HD00-PAGE.
*    伝票枚数
     MOVE     CNT-DENPYO         TO   HD03-DENPYO.
*    明細件数
     MOVE     CNT-MEISAI         TO   HD03-KENSU.
*
     WRITE    PRT-REC          FROM   HEAD-00    AFTER    2.
     WRITE    PRT-REC          FROM   HEAD-01    AFTER    15.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-03    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-04    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-02    AFTER    1.
     WRITE    PRT-REC          FROM   HEAD-01    AFTER    1.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    CVCSG001.
     CLOSE    PRINTF.
*
 END-EXIT.
     EXIT.

```
