****************************************************************
*         SYSTEM･･･ＤＣＭＪＡＰＡＮ物品受領書データ作成        *
*        PG-NAME･･･物品受領書データ作成                        *
*          PG-ID･･･SSY8731B                                    *
*                                            DATE. 07.05.29    *
*                                              BY. NAV         *
* 2018/06/14 NAV TAKAHASHI 発注数／１００を廃止                *
* 2019/09/19 NAV TAKAHASHI 消費税軽減税率対応　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY8731B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/29.
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
     SELECT   CVCSG001           ASSIGN    TO   CVCSG001
                                 ACCESS    MODE IS   SEQUENTIAL
                                 FILE STATUS    IS   DEN-STATUS.
*    物品受領書ファイル
     SELECT   DCMJYUF            ASSIGN    TO   DA-01-VI-DCMJYUL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYU-F00  JYU-F01
                                                JYU-F02  JYU-F03
                                                JYU-F04  JYU-F06
                                                JYU-F07
                                 STATUS         JYU-STATUS.
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
*****<物品受領ファイル>*****
 FD  DCMJYUF            LABEL RECORD   IS   STANDARD.
     COPY     DCMJYUF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
***  ｽﾃｰﾀｽ ｴﾘｱ
 01  STATUS-AREA.
     03  DEN-STATUS               PIC  X(02).
     03  JYU-STATUS               PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE   ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE   ZERO.
     03  SYORI-FLG                PIC  X(03)     VALUE   SPACE.
***  ｶｳﾝﾄ ｴﾘｱ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(02)     VALUE   ZERO.
     03  CNT-AFTER                PIC  9(02)     VALUE   ZERO.
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
 01  DCMJYUF-INV-FLG              PIC  X(03)     VALUE   SPACE.
 01  WK-TORICD                    PIC  X(06).
 01  FILLER                       REDEFINES      WK-TORICD.
     03  WK-TORICD1               PIC  9(06).
*
 01  FILE-ERR010                  PIC  N(10)     VALUE
         NC"受信Ｆ　　　異常！！".
 01  FILE-ERR020                  PIC  N(10)     VALUE
         NC"物品受領ＤＴ異常！！".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY8731B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(01).
     03  WK-DEPB03          PIC  9(07).
     03  WK-DEPB04          PIC  9(06).
     03  WK-DEPB05          PIC  9(06).
     03  WK-DEPB06.
         05  WK-DEPB061     PIC  9(06).
         05  WK-DEPB062     PIC  9(02).
     03  WK-DEPB07.
         05  WK-DEPB071     PIC  X(20).
         05  WK-DEPB072     PIC  X(20).
     03  WK-DEPB08.
         05  WK-DEPB081     PIC  9(04).
         05  WK-DEPB082     PIC  X(02).
     03  WK-DEPB09          PIC  X(15).
     03  WK-DEPB10.
         05  WK-DEPB101     PIC  9(03).
         05  WK-DEPB102     PIC  X(01).
     03  WK-DEPB11          PIC  9(02).
     03  WK-DEPB12          PIC  9(04).
     03  WK-DEPB13          PIC  9(02).
     03  WK-DEPB14          PIC  9(01).
     03  WK-DEPB15          PIC  9(01).
*#2019/09/19 NAV ST 消費税軽減税率対応
*****03  WK-DEPB16          PIC  X(23).
     03  WK-DEPB151         PIC  9(05).   *>税率
     03  WK-DEPB16          PIC  X(09).   *>空白
     03  WK-DEPB161         PIC  9(09).
*#2019/09/19 NAV ED 消費税軽減税率対応
     03  WK-DEPB17          PIC  X(01).

*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  9(01).
     03  WK-DEPD03.
         05  WK-DEPD031     PIC  9(07).
         05  WK-DEPD032     PIC  X(01).
     03  WK-DEPD04.
         05  WK-DEPD041     PIC  X(20).
         05  WK-DEPD042     PIC  X(20).
     03  WK-DEPD05          PIC  9(05)V9.
     03  WK-DEPD06          PIC  9(05)V9.
     03  WK-DEPD07          PIC  9(06)V99.
     03  WK-DEPD08          PIC  9(08).
     03  WK-DEPD09          PIC  9(06).
     03  WK-DEPD10          PIC  9(08).
     03  WK-DEPD11          PIC  X(13).
     03  WK-DEPD12          PIC  X(23).
*    合計レコード退避ワーク
 01  WK-DEPT-REC.
     03  WK-DEPT01          PIC  X(01).
     03  WK-DEPT02          PIC  9(01).
     03  WK-DEPT03          PIC  9(08).
     03  WK-DEPT04          PIC  9(08).
     03  WK-DEPT05          PIC  X(110).
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
     USE      AFTER     EXCEPTION   PROCEDURE    CVCSG001.
     MOVE     "CVCSG001"       TO   ERR-FL-ID.
     MOVE      DEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     DISPLAY   FILE-ERR010   UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
*--- << 物品受領ﾃﾞｰﾀ >> ---*
 000-DCMJYUF-ERR        SECTION.
     USE      AFTER     EXCEPTION   PROCEDURE    DCMJYUF.
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
     OPEN     INPUT     CVCSG001.
     OPEN     I-O       DCMJYUF.
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
     IF       DEN-01    =   "H"
              MOVE      DEN-REC        TO   WK-DEPB-REC
              GO        TO   MAIN-000
     END-IF.
     IF       DEN-01    =   "L"
              MOVE      DEN-REC        TO   WK-DEPD-REC
              MOVE      SPACE          TO   JYU-REC
              INITIALIZE                    JYU-REC
*             取引先コード
**************MOVE      PARA-TORICD(3:6)  TO  JYU-F00
              MOVE      PARA-TORICD(3:6)  TO  WK-TORICD
              MOVE      WK-TORICD1        TO  JYU-F00
*             受信日
              MOVE      WK-DATE        TO   JYU-F01
*             発注日
              MOVE      "3"            TO   LINK-IN-KBN
              MOVE      WK-DEPB04      TO   LINK-IN-YMD6
              MOVE      ZERO           TO   LINK-IN-YMD8
              MOVE      ZERO           TO   LINK-OUT-RET
              MOVE      ZERO           TO   LINK-OUT-YMD
              CALL      "SKYDTCKB"  USING   LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD
              MOVE      LINK-OUT-YMD   TO   JYU-F02
*             納品日
              MOVE      "3"            TO   LINK-IN-KBN
              MOVE      WK-DEPB05      TO   LINK-IN-YMD6
              MOVE      ZERO           TO   LINK-IN-YMD8
              MOVE      ZERO           TO   LINK-OUT-RET
              MOVE      ZERO           TO   LINK-OUT-YMD
              CALL      "SKYDTCKB"  USING   LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD
              MOVE      LINK-OUT-YMD   TO   JYU-F03
*             店舗ＣＤ
              MOVE      WK-DEPB081     TO   JYU-F04
*             店舗名
              MOVE      WK-DEPB09      TO   JYU-F05
*             伝票番号
              MOVE      WK-DEPB03      TO   JYU-F06
*             行番号
              MOVE      WK-DEPD02      TO   JYU-F07
***           DISPLAY "JYU-F00 = " JYU-F00 UPON CONS
***           DISPLAY "JYU-F01 = " JYU-F01 UPON CONS
***           DISPLAY "JYU-F02 = " JYU-F02 UPON CONS
***           DISPLAY "JYU-F03 = " JYU-F03 UPON CONS
***           DISPLAY "JYU-F04 = " JYU-F04 UPON CONS
***           DISPLAY "JYU-F05 = " JYU-F05 UPON CONS
***           DISPLAY "JYU-F06 = " JYU-F06 UPON CONS
***           DISPLAY "JYU-F07 = " JYU-F07 UPON CONS
              PERFORM DCMJYUF-READ-SEC
              IF  DCMJYUF-INV-FLG  =  "INV"
                 MOVE      DEN-REC        TO   WK-DEPD-REC
                 MOVE      SPACE          TO   JYU-REC
                 INITIALIZE                    JYU-REC
*                取引先コード
*****************MOVE      PARA-TORICD(3:6)  TO  JYU-F00
                 MOVE      PARA-TORICD(3:6)  TO  WK-TORICD
                 MOVE      WK-TORICD1        TO  JYU-F00
*                受信日
                 MOVE      WK-DATE        TO   JYU-F01
*                発注日
                 MOVE      "3"            TO   LINK-IN-KBN
                 MOVE      WK-DEPB04      TO   LINK-IN-YMD6
                 MOVE      ZERO           TO   LINK-IN-YMD8
                 MOVE      ZERO           TO   LINK-OUT-RET
                 MOVE      ZERO           TO   LINK-OUT-YMD
                 CALL      "SKYDTCKB"  USING   LINK-IN-KBN
                                               LINK-IN-YMD6
                                               LINK-IN-YMD8
                                               LINK-OUT-RET
                                               LINK-OUT-YMD
                 MOVE      LINK-OUT-YMD   TO   JYU-F02
*                納品日
                 MOVE      "3"            TO   LINK-IN-KBN
                 MOVE      WK-DEPB05      TO   LINK-IN-YMD6
                 MOVE      ZERO           TO   LINK-IN-YMD8
                 MOVE      ZERO           TO   LINK-OUT-RET
                 MOVE      ZERO           TO   LINK-OUT-YMD
                 CALL      "SKYDTCKB"  USING   LINK-IN-KBN
                                               LINK-IN-YMD6
                                               LINK-IN-YMD8
                                               LINK-OUT-RET
                                               LINK-OUT-YMD
                  MOVE      LINK-OUT-YMD   TO   JYU-F03
*                 店舗ＣＤ
                  MOVE      WK-DEPB081     TO   JYU-F04
*                 店舗名
                  MOVE      WK-DEPB09      TO   JYU-F05
*                 伝票番号
                  MOVE      WK-DEPB03      TO   JYU-F06
*                 行番号
                  MOVE      WK-DEPD02      TO   JYU-F07
*                 伝票区分
                  MOVE      WK-DEPB11      TO   JYU-F08
*                 商品ＣＤ
                  MOVE      WK-DEPD031     TO   JYU-F09
*                 商品名１、２
                  MOVE      WK-DEPD041     TO   JYU-F101
                  MOVE      WK-DEPD042     TO   JYU-F102
*                 発注数
*#2018/06/14 NAV ST 発注数／１００を廃止
*以前よりコメント*MOVE      WK-DEPD05      TO   JYU-F11
******************COMPUTE JYU-F11 = WK-DEPD05 / 100
                  MOVE      WK-DEPD05      TO   JYU-F11
*#2018/06/14 NAV ED 発注数／１００を廃止
*                 検品数
                  MOVE      WK-DEPD06      TO   JYU-F12
*                 原価単価
                  MOVE      WK-DEPD07      TO   JYU-F13
*                 原価金額
                  MOVE      WK-DEPD08      TO   JYU-F14
*                 ＪＡＮＣＤ
                  MOVE      WK-DEPD11      TO   JYU-F15
*#2019/09/19 NAV ST
******************MOVE      WK-DEPD12(1:9) TO   JYU-F16
                  IF  PARA-TORICD  =  880  OR  882  OR  883  OR
                                      1427 OR  14272 OR 14273
                      MOVE WK-DEPD12(1:7)  TO   JYU-F16
                  ELSE
                      MOVE WK-DEPD12(1:9)  TO   JYU-F16
                  END-IF
*#2019/09/19 NAV ED
*#2019/09/19 NAV ST 消費税軽減税率対応
                  MOVE     WK-DEPB151      TO   JYU-F17
                  IF  WK-DEPB151 NOT NUMERIC
                      MOVE  ZERO           TO   JYU-F17
                  END-IF
*#2019/09/19 NAV ED 消費税軽減税率対応
*                 レコード出力
                  WRITE     JYU-REC
                  ADD       1              TO   WRITE-CNT
              END-IF
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
     READ     CVCSG001  AT        END
              MOVE     "END"      TO   END-FLG
              GO                  TO   FL-READ-EXIT
              NOT  AT  END
              ADD       1         TO   READ-CNT
     END-READ.
*
     IF       DEN-01  =  "H" OR "L"
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
 DCMJYUF-READ-SEC       SECTION.
*
     READ     DCMJYUF
              INVALID      MOVE  "INV"   TO   DCMJYUF-INV-FLG
              NOT INVALID  MOVE  SPACE   TO   DCMJYUF-INV-FLG
     END-READ.
*
 DCMJYUF-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                              3.0        *
****************************************************************
 END-SEC                SECTION.
*
*    ファイルのＣＬＯＳＥ
     CLOSE    CVCSG001  DCMJYUF.
*
     DISPLAY "READ-CNT  = " READ-CNT  UPON CONS.
     DISPLAY "TAIS-CNT  = " TAIS-CNT  UPON CONS.
     DISPLAY "WRITE-CNT = " WRITE-CNT UPON CONS.
*
 END-EXIT.
     EXIT.
