****************************************************************
*         SYSTEM･･･リック物品受領書データ作成                  *
*        PG-NAME･･･物品受領書データ作成                        *
*          PG-ID･･･SJH8903B                                    *
*                                            DATE. 08.05.02    *
*                                              BY. NAV         *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SJH8903B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/05/02.
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
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*    検収データＦ
     SELECT   LIJFILE           ASSIGN    TO   LIJFILE
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   LIJ-STATUS.
*    物品受領書ファイル
     SELECT   DCMJYRF            ASSIGN    TO   DA-01-VI-DCMJYRL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYU-F00  JYU-F01
                                                JYU-F02  JYU-F03
                                                JYU-F04  JYU-F05
                                                JYU-F06  JYU-F07
                                 STATUS         JYU-STATUS.
*--------------------------------------------------------------*
 DATA                   DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*****＜伝票データＦ＞*****
*    FILE = ｹﾝｼｭｳﾃﾞｰﾀﾌｱｲﾙ
 FD  LIJFILE
                        BLOCK CONTAINS      4    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  LIJ-REC.
     03  LIJ-01                   PIC  X(02).
     03  LIJ-02                   PIC  X(849).
*
*****<物品受領ファイル>*****
 FD  DCMJYRF            LABEL RECORD   IS   STANDARD.
     COPY     DCMJYRF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*受領データ（ヘッダ）
     COPY     RCJYRHDF  OF   XFDLIB   JOINING   JHD    PREFIX.
*受領データ（明細）
     COPY     RCJYRMEF  OF   XFDLIB   JOINING   JME    PREFIX.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  LIJ-STATUS               PIC  X(02).
     03  JYU-STATUS               PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  IX                       PIC  9(02)     VALUE   ZERO.
     03  IY                       PIC  9(02)     VALUE   ZERO.
     03  CNT-GYO                  PIC  9(02)     VALUE   ZERO.
     03  READ-CNT                 PIC  9(07)     VALUE   ZERO.
     03  TAIS-CNT                 PIC  9(07)     VALUE   ZERO.
     03  WRITE-CNT                PIC  9(07)     VALUE   ZERO.
***  日付エリア
 01  WK-SYS-DATE.
     03  WK-YY1                   PIC  9(02)     VALUE   ZERO.
     03  WK-YMD.
         05  WK-YY2               PIC  9(02)     VALUE   ZERO.
         05  WK-MM                PIC  9(02)     VALUE   ZERO.
         05  WK-DD                PIC  9(02)     VALUE   ZERO.
 01  WK-DATE                      PIC  9(08)     VALUE   ZERO.
 01  CNT-PAGE                     PIC  9(04)     VALUE   ZERO.
***  ﾜｰｸ  ｴﾘｱ
 01  WRK-AREA.
     03  WRK-MAI                  PIC  9(06)     VALUE   ZERO.
     03  RD-SW                    PIC  9(01)     VALUE   ZERO.
     03  PAGE-SW                  PIC  9(01)     VALUE   ZERO.
 01  DCMJYRF-INV-FLG              PIC  X(03)     VALUE   SPACE.
*
 01  WK-HENKAN-10.
     03  WK-HEN-10                PIC  X(10).
     03  WK-HEN-10-R              REDEFINES WK-HEN-10.
         05  WK-HEN-10-H          PIC  9(10).
     03  WK-HEN-10-FLG            PIC  X(01)     VALUE   SPACE.
     03  WK-HEN-10-OK             PIC S9(07)V9(02) VALUE   ZERO.
*
 01  WK-HENKAN-4.
     03  WK-HEN-4                 PIC  X(05)     VALUE   ZERO.
     03  WK-HEN-4-R               REDEFINES WK-HEN-4.
         05  WK-HEN-4-H           PIC  9(05).
     03  WK-HEN-4-FLG             PIC  X(01)     VALUE   SPACE.
     03  WK-HEN-4-OK              PIC S9(04)     VALUE   ZERO.
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"受信Ｆ　　　異常！！".
 01  FILE-ERR020                  PIC  N(10)     VALUE
         NC"物品受領ＤＴ異常！！".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SJH8903B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN            PIC X(01).
 01  LINK-IN-YMD6           PIC 9(06).
 01  LINK-IN-YMD8           PIC 9(08).
 01  LINK-OUT-RET           PIC X(01).
 01  LINK-OUT-YMD           PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TORICD.
******************************************************************
*                       SHORI                         0.0.0      *
******************************************************************
 DECLARATIVES.
*--- << 受信ﾃﾞｰﾀ ｴﾗｰ >> ---*
 000-DENP-ERR           SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    LIJFILE.
     MOVE     "LIJFILE"       TO   ERR-FL-ID.
     MOVE      LIJ-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 物品受領ﾃﾞｰﾀ >> ---*
 000-DCMJYRF-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DCMJYRF.
     MOVE     "DCMJYUL1"       TO   ERR-FL-ID.
     MOVE      JYU-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR020   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 END DECLARATIVES.
******************************************************************
*            M  A  I  N          M  O  D  U  L  E                *
******************************************************************
 CONTROL-START          SECTION.
*
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC
                        UNTIL     END-FLG  =  "END".
     PERFORM            END-SEC.
*
 CONTROL-END.
     STOP     RUN.
****************************************************************
*             初期処理                              1.0        *
****************************************************************
 INIT-SEC               SECTION.
*    ファイルのＯＰＥＮ
     OPEN     INPUT     LIJFILE.
     OPEN     I-O       DCMJYRF.
*    ワーク初期化
     MOVE     SPACE              TO   END-FLG.
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
*    受信ファイル読込み
     PERFORM  FL-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
*    ヘッダ情報退避
     IF       LIJ-01    =   "HD"
              MOVE      LIJ-REC        TO   JHD-REC
              GO        TO   MAIN-000
     END-IF.
     IF       LIJ-01    =   "DT"
              MOVE      LIJ-REC        TO   JME-REC
              PERFORM HENSYU-SEC
     END-IF.
*
 MAIN-000.
*    受信データ読込み
     PERFORM       FL-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 FL-READ-SEC            SECTION.
*    伝票データ読込み
 READ-000.
     READ     LIJFILE  AT        END
              MOVE     "END"      TO   END-FLG
              GO                  TO   FL-READ-EXIT
              NOT  AT  END
              ADD       1         TO   READ-CNT
     END-READ.
*
     IF       LIJ-01  =  "HD" OR "DT"
              ADD       1         TO   TAIS-CNT
     ELSE
              GO                  TO   READ-000
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 HENSYU-SEC             SECTION.
*
     MOVE      SPACE          TO   JYU-REC.
     INITIALIZE                    JYU-REC.
*    取引先コード
     MOVE      JHD-F13        TO   JYU-F00.
*    受信日
     MOVE      WK-DATE        TO   JYU-F01
*    発注日
     MOVE      ZERO           TO   JYU-F02.
*    納品日
     MOVE      JHD-F03        TO   JYU-F03.
*    店舗ＣＤ
     MOVE      JHD-F12        TO   JYU-F04.
*    店舗名
     MOVE      JHD-F19        TO   JYU-F05.
*    伝票番号
     MOVE      JHD-F02        TO   JYU-F06.
*    行番号
     MOVE      JME-F02        TO   JYU-F07.
     PERFORM DCMJYRF-READ-SEC.
     IF  DCMJYRF-INV-FLG  =  "INV"
                 MOVE      SPACE          TO   JYU-REC
                 INITIALIZE                    JYU-REC
*                取引先コード
                 MOVE      JHD-F13        TO   JYU-F00
*                受信日
                 MOVE      WK-DATE        TO   JYU-F01
*                発注日
                 MOVE      ZERO           TO   JYU-F02
*                納品日
                 MOVE      JHD-F03        TO   JYU-F03
*                店舗ＣＤ
                 MOVE      JHD-F12        TO   JYU-F04
*                店舗名
                 MOVE      JHD-F19        TO   JYU-F05
*                伝票番号
                 MOVE      JHD-F02        TO   JYU-F06
*                行番号
                 MOVE      JME-F02        TO   JYU-F07
*                伝票区分
                 MOVE      JHD-F08        TO   JYU-F08
*                商品ＣＤ
                 MOVE      ZERO           TO   JYU-F09
*                 商品名１、２
                 MOVE      JME-F10        TO   JYU-F101
                 MOVE      JME-F12        TO   JYU-F102
*                発注数
                 INITIALIZE                    WK-HENKAN-4
                 PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 5
                   IF  JME-F07(IX:1) = "-"
                       MOVE  "1"    TO   WK-HEN-4-FLG
                       MOVE  "0"    TO   WK-HEN-4(IX:1)
                   ELSE
                       MOVE JME-F07(IX:1) TO WK-HEN-4(IX:1)
                   END-IF
                 END-PERFORM
                 IF    WK-HEN-4-FLG = "1"
                       COMPUTE JYU-F11 = WK-HEN-4-H * -1
                 ELSE
                       MOVE    WK-HEN-4-H TO  JYU-F11
                 END-IF
*****            DISPLAY "JYU-F11 = " JYU-F11  UPON CONS
*                検品数
                 MOVE      JME-F04        TO   JYU-F12
*                原価単価
                 INITIALIZE                    WK-HENKAN-10
                 MOVE       1              TO  IY
                 PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 11
                  IF JME-F05(IX:1) NOT = "."
                   IF  JME-F05(IX:1) = "-"
                       MOVE  "1"    TO   WK-HEN-10-FLG
                       MOVE  "0"    TO   WK-HEN-10(IY:1)
                   ELSE
                       MOVE JME-F05(IX:1) TO WK-HEN-10(IY:1)
                   END-IF
                   ADD        1     TO   IY
                  END-IF
                 END-PERFORM
                 IF    WK-HEN-10-FLG = "1"
                       COMPUTE JYU-F13 = (WK-HEN-10-H * -1 ) / 100
                 ELSE
                       COMPUTE JYU-F13 = WK-HEN-10-H / 100
                 END-IF
*                原価金額
                 COMPUTE JYU-F14 = JYU-F12 * JYU-F13
*                ＪＡＮＣＤ
                 MOVE      JME-F03        TO   JYU-F15
*                レコード出力
                  WRITE     JYU-REC
                  ADD       1              TO   WRITE-CNT
     END-IF.
*
 HENSYU-EXIT.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                  2.5.1      *
****************************************************************
 DCMJYRF-READ-SEC       SECTION.
*
     READ     DCMJYRF
              INVALID      MOVE  "INV"   TO   DCMJYRF-INV-FLG
              NOT INVALID  MOVE  SPACE   TO   DCMJYRF-INV-FLG
     END-READ.
*
 DCMJYRF-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    LIJFILE  DCMJYRF.
*
     DISPLAY "READ-CNT  = " READ-CNT  UPON CONS.
     DISPLAY "TAIS-CNT  = " TAIS-CNT  UPON CONS.
     DISPLAY "WRITE-CNT = " WRITE-CNT UPON CONS.
*
 END-EXIT.
     EXIT.
