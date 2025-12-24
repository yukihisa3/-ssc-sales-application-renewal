# SSY3751T

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3751T.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　発注処理　　　　　　　　　　　　　*
*    モジュール名　　　　：　管理番号リスト出力　　　　　　　　*
*    作成日／更新日　　　：　2010/10/08                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3751T.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/08/10.
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
         YB        IS   PITCH-15       *> 1.5ピッチ
         YB-21     IS   PITCH-15-YKBAI *> 1.5ピッチ、横倍
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ナフコ管理番号Ｆ >>--*
     SELECT  NFKANRF   ASSIGN TO     DA-01-VI-NFKANRL1
                       ORGANIZATION  INDEXED
                       ACCESS MODE   SEQUENTIAL
                       RECORD KEY    KAN-F01
                                     KAN-F02
                                     KAN-F03
                       FILE STATUS   KAN-ST.
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
*----<< ナフコ管理番号Ｆ >>--*
 FD  NFKANRF  LABEL RECORD  IS  STANDARD.
     COPY  NFKANRF OF XFDLIB
           JOINING  KAN  AS PREFIX.

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
 01  KAN-ST            PIC  X(02).
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
     03  FILLER  PIC  X(08)  VALUE  "SSY3751T".
     03  FILLER  PIC  X(31)  VALUE  SPACE.
     03  FILLER  PIC  N(11)  CHARACTER TYPE  PITCH-20-YKBAI
       VALUE  NC"＜ナフコ管理番号結果＞".
     03  FILLER  PIC  X(30)   VALUE  SPACE.
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

 01  KUGIRI-SEN.
     03  FILLER  PIC  X(136) VALUE  ALL "-".

 01  MEIS01  CHARACTER TYPE  PITCH-20-YKBAI.
     03  FILLER             PIC  X(09) VALUE SPACE.
     03  FILLER             PIC  N(02) VALUE  NC"この".
     03  MS01-SHUBETU       PIC  N(05).
     03  FILLER             PIC  N(09)
                            VALUE NC"データの管理番号は".
     03  FILLER             PIC  X(04).
     03  MS01-KANRINO       PIC  N(08).
     03  FILLER             PIC  N(03)   VALUE  NC"です。".

 01  MEIS02  CHARACTER TYPE PITCH-15.
     03  FILLER             PIC  X(34) VALUE SPACE.
     03  FILLER             PIC  N(06) VALUE NC"作成担当者：".
     03  MS02-TANCD         PIC  X(02).
     03  FILLER             PIC  X(02) VALUE SPACE.
     03  MS02-TANNAME       PIC  N(10).
     03  FILLER             PIC  X(02) VALUE SPACE.
     03  FILLER             PIC  X(08) VALUE "ﾊﾞｯﾁNO.:".
     03  MS02-BACH-NO       PIC  9(08).
     03  FILLER             PIC  X(01) VALUE "-".
     03  MS02-BACH-TIME     PIC  9(04).
     03  FILLER             PIC  X(01) VALUE "-".
     03  MS02-BACH-TORICD   PIC  9(08).

 01  MEIS03.
     03  FILLER             PIC  X(44) VALUE SPACE.
     03  FILLER             PIC  X(08) VALUE "ﾊﾞｯﾁNO.:".
     03  MS03-BACH-NO       PIC  9(08).
     03  FILLER             PIC  X(01) VALUE "-".
     03  MS03-BACH-TIME     PIC  9(04).
     03  FILLER             PIC  X(01) VALUE "-".
     03  MS03-BACH-TORICD   PIC  9(08).
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
 01  PARA-KBN           PIC   X(01).
 01  PARA-KANRINO       PIC   9(08).
 01  PARA-HIDUKE        PIC   9(08).
 01  PARA-JIKAN         PIC   9(04).
 01  PARA-TOKCD         PIC   9(08).
 01  PARA-BUMON         PIC   X(04).
 01  PARA-KEKKA         PIC   X(01).
*
****************************************************************
 PROCEDURE              DIVISION   USING   PARA-KBN
                                           PARA-KANRINO
                                           PARA-HIDUKE
                                           PARA-JIKAN
                                           PARA-TOKCD
                                           PARA-BUMON
                                           PARA-KEKKA.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ナフコ管理番号Ｆ >>--*
 KAN-ERR                   SECTION.
     USE AFTER EXCEPTION PROCEDURE  NFKANRF.
     ACCEPT SYS-DATE  FROM DATE.
     ACCEPT SYS-TIME  FROM TIME.
     DISPLAY  "### SSY3751T NFKANRF ERROR STS=" KAN-ST " "
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
     DISPLAY  "### SSY3751T HTANMS ERROR STS=" HTANMS-ST " "
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

     DISPLAY  "*** SSY3751T START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
              UPON CONS.
*
     MOVE  SPACE                 TO  WK-KANRNO-X.
     MOVE  SPACE                 TO  WK-KANRNO-N.
*
     OPEN  INPUT  NFKANRF.
     OPEN  INPUT  HTANMS.
     OPEN  OUTPUT PRTF.
*
     MOVE  SPACE                TO   KAN-REC.
     INITIALIZE                      KAN-REC.
*
     MOVE  PARA-KBN             TO   KAN-F01.
     MOVE  PARA-KANRINO         TO   KAN-F02.
     MOVE  PARA-HIDUKE          TO   KAN-F031.
     MOVE  PARA-JIKAN           TO   KAN-F032.
     MOVE  PARA-TOKCD           TO   KAN-F033.
*
     START  NFKANRF   KEY  >=   KAN-F01  KAN-F02  KAN-F03
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
*    ナフコ管理番号ファイル読込　                              *
*--------------------------------------------------------------*
 INF-RD-SEC           SECTION.
     MOVE  "INF-RD-SEC"          TO  SEC-NAME.
*
     READ  NFKANRF
         AT  END
         MOVE  "99"              TO  PARA-KEKKA
         MOVE  "END"             TO  END-FLG
         GO    TO    INF-RD-EXIT
     END-READ.
*
*****DISPLAY "KAN-F01  = "  KAN-F01    UPON   CONS.
*    DISPLAY "KAN-F02  = "  KAN-F02    UPON   CONS.
*    DISPLAY "KAN-F031 = "  KAN-F031   UPON   CONS.
*    DISPLAY "KAN-F032 = "  KAN-F032   UPON   CONS.
*****DISPLAY "KAN-F033 = "  KAN-F033   UPON   CONS.
     IF  PARA-KBN       =   KAN-F01
     AND PARA-KANRINO   =   KAN-F02
     AND PARA-HIDUKE    =   KAN-F031
     AND PARA-JIKAN     =   KAN-F032
     AND PARA-TOKCD     =   KAN-F033
         CONTINUE
     ELSE
         MOVE  "99"              TO  PARA-KEKKA
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
     WRITE  PRT-REC  FROM HEAD01      AFTER 3.
     WRITE  PRT-REC  FROM KUGIRI-SEN  AFTER 2.
*
 HD-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印刷処理                                              *
*--------------------------------------------------------------*
 MS-PRT-SEC            SECTION.
     MOVE  "MS-PRT-SEC"          TO  SEC-NAME.
*
     IF   PARA-KBN  =  "1"
          MOVE   NC"オンライン"     TO  MS01-SHUBETU
     ELSE
          MOVE   NC"手書　　　"     TO  MS01-SHUBETU
     END-IF.
*管理番号変換(Xタイブ→Nタイプ)
     MOVE  SPACE                 TO  WK-KANRNO-X.
     MOVE  KAN-F02               TO  WK-KANRNO-X.
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
*
     MOVE  WK-KANRNO-N           TO  MS01-KANRINO.
*
     MOVE  KAN-F04               TO  MS02-TANCD.
     IF    KAN-F04    NOT  =   TAN-F01
           MOVE  PARA-BUMON      TO  TAN-F01
           MOVE  KAN-F04         TO  TAN-F02
           READ  HTANMS
             INVALID  KEY
                 MOVE  SPACE     TO  MS02-TANNAME
             NOT INVALID KEY
                 MOVE  TAN-F03   TO  MS02-TANNAME
           END-READ
     END-IF.
*
     MOVE  KAN-F031              TO  MS02-BACH-NO.
     MOVE  KAN-F032              TO  MS02-BACH-TIME.
     MOVE  KAN-F033              TO  MS02-BACH-TORICD.
*
     WRITE  PRT-REC  FROM MEIS01     AFTER 5.
     WRITE  PRT-REC  FROM MEIS02     AFTER 4.
*****WRITE  PRT-REC  FROM MEIS03     AFTER 4.
     WRITE  PRT-REC  FROM KUGIRI-SEN  AFTER 2.

     ADD  1  TO LINE-CNT.
 MS-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    終了処理                                                  *
*--------------------------------------------------------------*
 END-SEC            SECTION.
     MOVE  "END-SEC"             TO  SEC-NAME.
     CLOSE  NFKANRF.
     CLOSE  HTANMS.
     CLOSE  PRTF.
*
     ACCEPT  SYS-DATE  FROM  DATE.
     ACCEPT  SYS-TIME  FROM  TIME.
     DISPLAY  "*** SSY3751T EDIｶﾝﾘﾏｽﾀ ﾘｽﾄ PAGE = "  PAGE-CNT
              UPON CONS.
     DISPLAY  "*** SSY3751T END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
              UPON CONS.
 END-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
