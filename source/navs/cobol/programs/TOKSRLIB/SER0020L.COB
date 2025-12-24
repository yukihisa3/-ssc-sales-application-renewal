****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　システム共通サブシステム　　　　　*
*    モジュール名　　　　：　管理番号バッチ_リスト出力　　　　*
*    作成日／作成者　　　：　2021/11/16 INOUE                  *
*    処理概要　　　　　　：　管理番号・バッチ_をリスト出力　　*
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SER0020L.
*                      :SSY3751T.TOKSLIBS
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/11/16.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   PITCH-20       *> 2.0ピッチ
         YA-21     IS   PITCH-20-YKBAI *> 2.0ピッチ、横倍
         YA-22     IS   PITCH-20-BAI   *> 2.0ピッチ、倍角
         YB        IS   PITCH-15       *> 1.5ピッチ
         YB-21     IS   PITCH-15-YKBAI *> 1.5ピッチ、横倍
         YB-22     IS   PITCH-15-BAI   *> 1.5ピッチ、倍角
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< D365エラー情報Ｆ >>--*
     SELECT  ERRJYOL1  ASSIGN TO     DA-01-VI-ERRJYOL1
                       ORGANIZATION  INDEXED
                       ACCESS MODE   SEQUENTIAL
                       RECORD KEY    ERJ-F01
                       FILE STATUS   ERJ-ST.
*----<< 担当者マスタ >>--*
     SELECT   HTANMS   ASSIGN        DA-01-VI-TANMS1
                       ORGANIZATION  INDEXED
                       ACCESS MODE   RANDOM
                       RECORD KEY    TAN-F01  TAN-F02
                       STATUS        HTANMS-ST.
*----<< プリンタ >>-*
     SELECT   PRTF     ASSIGN        LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< D365エラー情報Ｆ >>--*
 FD  ERRJYOL1  LABEL RECORD  IS  STANDARD.
     COPY  ERRJYOL1 OF XFDLIB
           JOINING  ERJ  AS PREFIX.

*----<< 担当者マスタ >>--*
 FD  HTANMS  LABEL RECORD   IS   STANDARD.
     COPY  HTANMS OF XFDLIB
           JOINING  TAN  AS PREFIX.

*----<< プリンタ >>-*
 FD  PRTF  LABEL RECORD  IS OMITTED.
 01  PRT-REC            PIC  X(200).

*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03)  VALUE  SPACE.
     03  CHG-FLG        PIC  X(01)  VALUE  SPACE.
     03  FG-HTANMS-INV  PIC  9(01).
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)  VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)  VALUE  ZERO.
 01  IDX.
     03  IX1            PIC  9(03)  VALUE  ZERO.
     03  IX2            PIC  9(03)  VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  ERJ-ST            PIC  X(02).
 01  HTANMS-ST         PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).

 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).

 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).

*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH          PIC   9(02)  VALUE  ZERO.
     03  WK-MM          PIC   9(02)  VALUE  ZERO.


*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER  PIC  X(01)  VALUE  SPACE.
     03  FILLER  PIC  X(08)  VALUE  "SER0020L".
     03  FILLER  PIC  X(29)  VALUE  SPACE.
     03  FILLER  PIC  N(14)  CHARACTER TYPE  PITCH-20-YKBAI
         VALUE  NC"＜管理番号バッチＮＯリスト＞".
     03  FILLER  PIC  X(17)   VALUE  SPACE.
     03  HD01-Y  PIC  9999.
     03  FILLER  PIC  N(01)  VALUE  NC"年"
                             CHARACTER TYPE  PITCH-20.
     03  HD01-M  PIC  99.
     03  FILLER  PIC  N(01)  VALUE  NC"月"
                             CHARACTER TYPE  PITCH-20.
     03  HD01-D  PIC  99.
     03  FILLER  PIC  N(01)  VALUE  NC"日"
                             CHARACTER TYPE  PITCH-20.
     03  FILLER  PIC  X(02)  VALUE  SPACE.
     03  HD01-PG PIC  ZZ9.
     03  FILLER  PIC  N(01)  VALUE  NC"頁"
                             CHARACTER TYPE  PITCH-20.
 01  HEAD02.
     03  FILLER  PIC  X(48)  VALUE  SPACE.
     03  FILLER  PIC  N(13)
         VALUE  NC"（Ｄ３６５エラーリカバリ）"
                             CHARACTER TYPE  PITCH-20.
*
 01  KUGIRI-SEN1.
     03  FILLER  PIC  X(30)  VALUE  SPACE.
     03  FILLER  PIC  X(01)  VALUE  "+".
     03  FILLER  PIC  X(61)  VALUE  ALL "-".
     03  FILLER  PIC  X(01)  VALUE  "+".
*
 01  KUGIRI-SEN2.
     03  FILLER  PIC  X(30)  VALUE  SPACE.
     03  FILLER  PIC  X(01)  VALUE  "+".
     03  FILLER  PIC  X(61)  VALUE  SPACE.
     03  FILLER  PIC  X(01)  VALUE  "+".
*
 01  MEIS01  CHARACTER TYPE  PITCH-15-BAI.
     03  FILLER             PIC  X(30) VALUE SPACE.
     03  FILLER             PIC  X(01) VALUE "+".
     03  FILLER             PIC  X(09) VALUE SPACE.
     03  FILLER             PIC  N(05) VALUE NC"管理番号：".
     03  FILLER             PIC  X(02) VALUE SPACE.
     03  MS01-KANRINO       PIC  N(08).
     03  FILLER             PIC  X(11) VALUE SPACE.
     03  FILLER             PIC  X(01) VALUE "+".
*
 01  MEIS02  CHARACTER TYPE PITCH-15-BAI.
     03  FILLER             PIC  X(30) VALUE SPACE.
     03  FILLER             PIC  X(01) VALUE "+".
     03  FILLER             PIC  X(06) VALUE SPACE.
     03  FILLER             PIC  N(06) VALUE NC"バッチＮＯ：".
     03  FILLER             PIC  X(02) VALUE SPACE.
     03  MS02-BACH-NO1      PIC  N(04).
     03  FILLER             PIC  N(01) VALUE NC"／".
     03  MS02-BACH-NO2      PIC  N(02).
     03  FILLER             PIC  N(01) VALUE NC"／".
     03  MS02-BACH-NO3      PIC  N(02).
     03  FILLER             PIC  X(05) VALUE SPACE.
     03  FILLER             PIC  X(01) VALUE "+".
*
 01  MEIS03  CHARACTER TYPE PITCH-15-BAI.
     03  FILLER             PIC  X(30) VALUE SPACE.
     03  FILLER             PIC  X(01) VALUE "+".
     03  FILLER             PIC  X(26) VALUE SPACE.
     03  MS03-BACH-T1       PIC  N(02).
     03  FILLER             PIC  N(01) VALUE NC"：".
     03  MS03-BACH-T2       PIC  N(02).
     03  FILLER             PIC  N(01) VALUE NC"：".
     03  MS03-BACH-T3       PIC  N(02).
     03  FILLER             PIC  X(11) VALUE SPACE.
     03  FILLER             PIC  X(01) VALUE "+".
*
 01  MEIS04.
     03  FILLER             PIC  X(30) VALUE SPACE.
     03  FILLER             PIC  X(01) VALUE "+".
     03  FILLER             PIC  X(06) VALUE SPACE.
     03  FILLER             PIC  N(06) VALUE NC"取込担当者："
                                 CHARACTER TYPE PITCH-15-BAI.
     03  FILLER             PIC  X(02) VALUE SPACE.
     03  MS03-TANCD         PIC  X(02).
     03  FILLER             PIC  X(02) VALUE SPACE.
     03  MS03-TANNAME       PIC  N(10)
                                 CHARACTER TYPE PITCH-20.
     03  FILLER             PIC  X(11) VALUE SPACE.
     03  FILLER             PIC  X(01) VALUE "+".
*
 01  WK-CHG.
     05  FILLER         PIC   X(01)  VALUE "0".
     05  FILLER         PIC   N(01)  VALUE NC"０".
     05  FILLER         PIC   X(01)  VALUE "1".
     05  FILLER         PIC   N(01)  VALUE NC"１".
     05  FILLER         PIC   X(01)  VALUE "2".
     05  FILLER         PIC   N(01)  VALUE NC"２".
     05  FILLER         PIC   X(01)  VALUE "3".
     05  FILLER         PIC   N(01)  VALUE NC"３".
     05  FILLER         PIC   X(01)  VALUE "4".
     05  FILLER         PIC   N(01)  VALUE NC"４".
     05  FILLER         PIC   X(01)  VALUE "5".
     05  FILLER         PIC   N(01)  VALUE NC"５".
     05  FILLER         PIC   X(01)  VALUE "6".
     05  FILLER         PIC   N(01)  VALUE NC"６".
     05  FILLER         PIC   X(01)  VALUE "7".
     05  FILLER         PIC   N(01)  VALUE NC"７".
     05  FILLER         PIC   X(01)  VALUE "8".
     05  FILLER         PIC   N(01)  VALUE NC"８".
     05  FILLER         PIC   X(01)  VALUE "9".
     05  FILLER         PIC   N(01)  VALUE NC"９".
*
 01  TBL-NAT                REDEFINES  WK-CHG.
     03  TBL-NATR           OCCURS  10.
         05  TBL-NAT1       PIC   X(01).
         05  TBL-NAT2       PIC   N(01).
*
 01  WK-KANRNO-X.
     03  WK-KANRINOR-X      OCCURS  8.
         05  WK-KANRI-X     PIC   X(01).
*
 01  WK-KANRNO-N.
     03  WK-KANRINOR-N      OCCURS  8.
         05  WK-KANRI-N     PIC   N(01).
*
 01  WK-BATCHD-X.
     03  WK-BATCHDT-X       OCCURS  8.
         05  WK-BATDT-X     PIC   X(01).
*
 01  WK-BATCHD-N.
     03  WK-BATCHDT-N       OCCURS  8.
         05  WK-BATDT-N     PIC   N(01).
 01  WK-BATCHD-R REDEFINES  WK-BATCHD-N.
     03  WK-BATCHD-N1       PIC   N(04).
     03  WK-BATCHD-N2       PIC   N(02).
     03  WK-BATCHD-N3       PIC   N(02).
*
 01  WK-BATCHT-X.
     03  WK-BATCHTM-X       OCCURS  6.
         05  WK-BATTM-X     PIC   X(01).
*
 01  WK-BATCHT-N.
     03  WK-BATCHTM-N       OCCURS  6.
         05  WK-BATTM-N     PIC   N(01).
 01  WK-BATCHT-R REDEFINES  WK-BATCHT-N.
     03  WK-BATCHT-N1       PIC   N(02).
     03  WK-BATCHT-N2       PIC   N(02).
     03  WK-BATCHT-N3       PIC   N(02).
*
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE                SECTION.
*
 01  PARA-KANRINO       PIC   9(08).
*
****************************************************************
 PROCEDURE              DIVISION   USING
                                           PARA-KANRINO.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< D365エラー情報Ｆ >>--*
 ERJ-ERR                   SECTION.
     USE AFTER EXCEPTION PROCEDURE  ERRJYOL1.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SER0020L ERRJYOL1 ERROR STS=" ERJ-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     MOVE     4000           TO     PROGRAM-STATUS.
     STOP RUN.
*----<< 担当者マスタ >>--*
 HTANMS-ERR             SECTION.
     USE AFTER EXCEPTION PROCEDURE  HTANMS.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SER0020L HTANMS ERROR STS=" HTANMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME  UPON CONS.
     MOVE     4000           TO     PROGRAM-STATUS.
     STOP RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    全体処理                                                  *
*--------------------------------------------------------------*
 PROG-CNTL          SECTION.
     MOVE    "PROG-CNTL"      TO   SEC-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    初期処理                                                  *
*--------------------------------------------------------------*
 INIT-SEC           SECTION.
     MOVE  "INIT-SEC"            TO  SEC-NAME.
     DISPLAY "## PARA-KANRINO = " PARA-KANRINO UPON CONS.
     ACCEPT  SYS-DATE  FROM  DATE.
     MOVE "3"                    TO  LINK-IN-KBN.
     MOVE SYS-DATE               TO  LINK-IN-YMD6.
     CALL "SKYDTCKB"  USING  LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     IF  LINK-OUT-RET = ZERO
         MOVE LINK-OUT-YMD       TO  SYS-DATEW
     ELSE
         MOVE ZERO               TO  SYS-DATEW
     END-IF.

     ACCEPT  SYS-TIME  FROM  TIME.

     DISPLAY  "*** SER0020L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
              UPON CONS.
*
     MOVE  SPACE                 TO  WK-KANRNO-X.
     MOVE  SPACE                 TO  WK-KANRNO-N.
*
     OPEN  INPUT  ERRJYOL1.
     OPEN  INPUT  HTANMS.
     OPEN  OUTPUT PRTF.
*
     MOVE  SPACE                TO   ERJ-REC.
     INITIALIZE                      ERJ-REC.
*
     MOVE  PARA-KANRINO         TO   ERJ-F01.
*
     START  ERRJYOL1   KEY  >=   ERJ-F01
            INVALID   KEY
            MOVE     "END"      TO   END-FLG
            GO                  TO   INIT-EXIT
     END-START.
*
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
     PERFORM  INF-RD-SEC.

 INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    D365エラー情報ファイル読込　                              *
*--------------------------------------------------------------*
 INF-RD-SEC           SECTION.
     MOVE  "INF-RD-SEC"          TO  SEC-NAME.
*
     READ  ERRJYOL1
         AT  END
         MOVE  "END"             TO  END-FLG
         GO    TO    INF-RD-EXIT
     END-READ.
*
     IF  PARA-KANRINO   =   ERJ-F01
         CONTINUE
     ELSE
         MOVE  "END"             TO  END-FLG
         GO    TO    INF-RD-EXIT
     END-IF.
*
 INF-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    主処理                                                    *
*--------------------------------------------------------------*
 MAIN-SEC           SECTION.
     MOVE  "MAIN-SEC"            TO  SEC-NAME.

     PERFORM  HD-PRT-SEC.
     PERFORM  MS-PRT-SEC.
     PERFORM  INF-RD-SEC.

 MAIN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印刷処理                                            *
*--------------------------------------------------------------*
 HD-PRT-SEC            SECTION.
     MOVE  "HD-PRT-SEC"          TO  SEC-NAME.
     MOVE  SYS-YYW               TO  HD01-Y.
     MOVE  SYS-MMW               TO  HD01-M.
     MOVE  SYS-DDW               TO  HD01-D.
*
*****MOVE SPACE               TO PRT-REC
*****WRITE  PRT-REC  AFTER PAGE
*
     ADD    1                    TO  PAGE-CNT.
*
     MOVE  PAGE-CNT              TO  HD01-PG.
*
     WRITE  PRT-REC  FROM HEAD01      AFTER 2.
     WRITE  PRT-REC  FROM HEAD02      AFTER 1.
     WRITE  PRT-REC  FROM KUGIRI-SEN1 AFTER 8.
     WRITE  PRT-REC  FROM KUGIRI-SEN2 AFTER 1.
*
 HD-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印刷処理                                              *
*--------------------------------------------------------------*
 MS-PRT-SEC            SECTION.
     MOVE  "MS-PRT-SEC"          TO  SEC-NAME.
*
*管理番号変換(Xタイブ→Nタイプ)
     MOVE  SPACE                 TO  WK-KANRNO-X.
     MOVE  ERJ-F01               TO  WK-KANRNO-X.
     PERFORM   VARYING  IX1  FROM 1  BY  1  UNTIL  IX1 > 8
       MOVE    SPACE             TO  CHG-FLG
       PERFORM  VARYING  IX2  FROM  1  BY  1
                         UNTIL  IX2 > 10  OR  CHG-FLG = "1"
         IF  WK-KANRI-X (IX1)  =    TBL-NAT1 (IX2)
             MOVE  TBL-NAT2 (IX2)   TO   WK-KANRI-N (IX1)
             MOVE  "1"              TO   CHG-FLG
         END-IF
       END-PERFORM
     END-PERFORM.
     MOVE  WK-KANRNO-N           TO  MS01-KANRINO.
*
*バッチ_日　変換(Xタイブ→Nタイプ)
     MOVE  SPACE                 TO  WK-BATCHD-X.
     MOVE  ERJ-F02               TO  WK-BATCHD-X.
     PERFORM   VARYING  IX1  FROM 1  BY  1  UNTIL  IX1 > 8
       MOVE    SPACE             TO  CHG-FLG
       PERFORM  VARYING  IX2  FROM  1  BY  1
                         UNTIL  IX2 > 10  OR  CHG-FLG = "1"
         IF  WK-BATDT-X (IX1)  =    TBL-NAT1 (IX2)
             MOVE  TBL-NAT2 (IX2)   TO   WK-BATDT-N (IX1)
             MOVE  "1"              TO   CHG-FLG
         END-IF
       END-PERFORM
     END-PERFORM.
     MOVE  WK-BATCHD-N1          TO  MS02-BACH-NO1.
     MOVE  WK-BATCHD-N2          TO  MS02-BACH-NO2.
     MOVE  WK-BATCHD-N3          TO  MS02-BACH-NO3.
*
*バッチ_時　変換(Xタイブ→Nタイプ)
     MOVE  SPACE                 TO  WK-BATCHT-X.
     MOVE  ERJ-F03               TO  WK-BATCHT-X.
     PERFORM   VARYING  IX1  FROM 1  BY  1  UNTIL  IX1 > 6
       MOVE    SPACE             TO  CHG-FLG
       PERFORM  VARYING  IX2  FROM  1  BY  1
                         UNTIL  IX2 > 10  OR  CHG-FLG = "1"
         IF  WK-BATTM-X (IX1)  =    TBL-NAT1 (IX2)
             MOVE  TBL-NAT2 (IX2)   TO   WK-BATTM-N (IX1)
             MOVE  "1"              TO   CHG-FLG
         END-IF
       END-PERFORM
     END-PERFORM.
     MOVE  WK-BATCHT-N1          TO  MS03-BACH-T1.
     MOVE  WK-BATCHT-N2          TO  MS03-BACH-T2.
     MOVE  WK-BATCHT-N3          TO  MS03-BACH-T3.
*
*担当者名取得
     MOVE  ERJ-F04               TO  TAN-F01.
     MOVE  ERJ-F05               TO  MS03-TANCD
                                     TAN-F02.
     READ  HTANMS
           INVALID  KEY
                 MOVE ALL NC"＊" TO  MS03-TANNAME
           NOT INVALID KEY
                 MOVE  TAN-F03   TO  MS03-TANNAME
     END-READ.
*
*プリントアウト
     WRITE  PRT-REC  FROM MEIS01      AFTER 2.
     WRITE  PRT-REC  FROM KUGIRI-SEN2 AFTER 1.
     WRITE  PRT-REC  FROM KUGIRI-SEN1 AFTER 1.
     WRITE  PRT-REC  FROM KUGIRI-SEN2 AFTER 1.
     WRITE  PRT-REC  FROM MEIS02      AFTER 2.
     WRITE  PRT-REC  FROM MEIS03      AFTER 3.
     WRITE  PRT-REC  FROM KUGIRI-SEN2 AFTER 1.
     WRITE  PRT-REC  FROM KUGIRI-SEN1 AFTER 1.
     WRITE  PRT-REC  FROM KUGIRI-SEN2 AFTER 1.
     WRITE  PRT-REC  FROM MEIS04      AFTER 2.
     WRITE  PRT-REC  FROM KUGIRI-SEN2 AFTER 1.
     WRITE  PRT-REC  FROM KUGIRI-SEN1 AFTER 1.

     ADD  1  TO LINE-CNT.
 MS-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    終了処理                                                  *
*--------------------------------------------------------------*
 END-SEC            SECTION.
     MOVE  "END-SEC"             TO  SEC-NAME.
     CLOSE  ERRJYOL1.
     CLOSE  HTANMS.
     CLOSE  PRTF.
*
     ACCEPT  SYS-DATE  FROM  DATE.
     ACCEPT  SYS-TIME  FROM  TIME.
     DISPLAY  "*** SER0020L ﾘｽﾄ PAGE = "  PAGE-CNT
              UPON CONS.
     DISPLAY  "*** SER0020L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
              UPON CONS.
 END-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
