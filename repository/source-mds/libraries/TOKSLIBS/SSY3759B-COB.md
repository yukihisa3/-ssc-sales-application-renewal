# SSY3759B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3759B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　出荷処理　　　　　　　　　　　*　
*    モジュール名　　　　：　出荷情報作成　　　　　　　　　*
*    作成日／更新日　　　：　10/10/14                      *
*    作成者／更新者　　　：　ＮＡＶ　阿部　　　　　　　　　*
*    処理概要　　　　　　：　基本情報Ｆより、箱数Ｆ、          *
*                            数量訂正Ｆ、を作成する。　　　　　*
*                            出力する　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3759B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*基本情報Ｆ（管理ＮＯ）
     SELECT   NFJOHOF1  ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JH1-F01   JH1-F05
                                            JH1-F06   JH1-F07
                                            JH1-F08   JH1-F09
                        FILE  STATUS   IS   JH1-STATUS.
*基本情報Ｆ（バッチＮＯ）
     SELECT   NFJOHOF2  ASSIGN    TO        DA-01-VI-NFJOHOL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JH2-F02   JH2-F03
                                            JH2-F04   JH2-F05
                                            JH2-F06   JH2-F07
                                            JH2-F08   JH2-F09
                        FILE  STATUS   IS   JH2-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*作場マスタ　　　
     SELECT   SAKUBAF   ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SAK-F01
                        FILE STATUS    IS   SAK-STATUS.
*ナフコ商品名称マスタ
     SELECT   NFMEIMS   ASSIGN    TO        DA-01-VI-NFMEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE STATUS    IS   MEI-STATUS.
*箱数ファイル
     SELECT   NFHAKOF   ASSIGN    TO        DA-01-VI-NFHAKOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HAK-F01   HAK-F05
                                            HAK-F06   HAK-F07
                                            HAK-F08
                        FILE STATUS    IS   HAK-STATUS.
*数量訂正ファイル
     SELECT   NFSUTEF   ASSIGN    TO        DA-01-VI-NFSUTEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       STE-F01   STE-F05
                                            STE-F06   STE-F07
                                            STE-F08   STE-F09
                        FILE  STATUS   IS   STE-STATUS.
*出荷情報エラーファイル
     SELECT   NFSERRF   ASSIGN    TO        DA-01-VI-NFSERRL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ERR-F01  ERR-F02
                                            ERR-F03  ERR-F04
                                            ERR-F05  ERR-F06
                                            ERR-F07  ERR-F08
                                            ERR-F09
                        FILE  STATUS   IS   ERR-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本情報ファイル（管理ＮＯ）
******************************************************************
 FD  NFJOHOF1           LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JH1       PREFIX.
******************************************************************
*    基本情報ファイル（バッチＮＯ）
******************************************************************
 FD  NFJOHOF2           LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JH2       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  NFMEIMS            LABEL RECORD   IS   STANDARD.
     COPY     NFMEIMS   OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    作場マスタ　　　
******************************************************************
 FD  SAKUBAF            LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF   OF        XFDLIB
              JOINING   SAK       PREFIX.
******************************************************************
*    箱数ファイル　　　　　
******************************************************************
 FD  NFHAKOF            LABEL RECORD   IS   STANDARD.
     COPY     NFHAKOF   OF        XFDLIB
              JOINING   HAK       PREFIX.
******************************************************************
*    数量訂正ファイル　　　
******************************************************************
 FD  NFSUTEF            LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEF   OF        XFDLIB
              JOINING   STE       PREFIX.
******************************************************************
*    出荷情報エラーファイル
******************************************************************
 FD  NFSERRF
                        LABEL RECORD   IS   STANDARD.
     COPY     NFSERRF   OF        XFDLIB
              JOINING   ERR  AS   PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*発注データ（ヘッダ）
     COPY     NFJOHOF   OF   XFDLIB   JOINING   JH3    PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  CHK-FLG                 PIC  X(01)     VALUE  ZERO.
 01  OUT-FLG                 PIC  X(01)     VALUE  ZERO.
 01  WK-ERKBN                PIC  X(04)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(09)     VALUE  ZERO.
 01  HAK-O-CNT               PIC  9(02)     VALUE  ZERO.
 01  HAK-R-CNT               PIC  9(09)     VALUE  ZERO.
 01  STE-O-CNT               PIC  9(09)     VALUE  ZERO.
 01  STE-R-CNT               PIC  9(09)     VALUE  ZERO.
 01  ERR-O-CNT               PIC  9(09)     VALUE  ZERO.
 01  JH1-R-CNT               PIC  9(09)     VALUE  ZERO.
 01  JH2-R-CNT               PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-HAKOSU         PIC 9(06)        VALUE  ZERO.
     03  WK-GENKA          PIC 9(05)V9(02)  VALUE  ZERO.
     03  WK-BAIKA          PIC 9(05)V9(02)  VALUE  ZERO.
 01  WK-GOKEI.
     03  WK-SURYO          PIC 9(06)V9(01)  VALUE  ZERO.
     03  WK-SOKONSU        PIC 9(06)V9(01)  VALUE  ZERO.
*ブレイクキー
 01  BRK-KEY1.
     03  BRK-F01           PIC 9(08)        VALUE  ZERO.
     03  BRK-F02           PIC 9(08)        VALUE  ZERO.
     03  BRK-F03           PIC 9(04)        VALUE  ZERO.
     03  BRK-F04           PIC 9(08)        VALUE  ZERO.
     03  BRK-F05           PIC X(02)        VALUE  SPACE.
     03  BRK-F06           PIC 9(05)        VALUE  ZERO.
     03  BRK-HE20          PIC X(01)        VALUE  SPACE.
     03  BRK-F08           PIC 9(08)        VALUE  ZERO.
*
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  JH1-STATUS        PIC  X(02).
     03  JH2-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SAK-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  HAK-STATUS        PIC  X(02).
     03  STE-STATUS        PIC  X(02).
     03  ERR-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3759B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3759B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3759B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT1.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " HAKOF = ".
         05  OUT-CNT1       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT11.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE "RE HAK = ".
         05  OUT-CNT11      PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT2.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " SUTEF = ".
         05  OUT-CNT2       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT22.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE "RE SUT = ".
         05  OUT-CNT22      PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT3.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " JYOHF1= ".
         05  OUT-CNT3       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT33.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " JYOHF2= ".
         05  OUT-CNT33      PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT4.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " SJOHER= ".
         05  OUT-CNT4       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-KBN           PIC   9(01).
 01  PARA-BACHI-YMD     PIC   9(08).
 01  PARA-BACHI-TIME    PIC   9(04).
 01  PARA-BACHI-TORICD  PIC   9(08).
 01  PARA-KANRINO       PIC   9(08).
 01  PARA-SAKUCD        PIC   9(02).
 01  PARA-SYKYMD        PIC   9(08).
 01  PARA-TENYMD        PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-KBN
                                       PARA-BACHI-YMD
                                       PARA-BACHI-TIME
                                       PARA-BACHI-TORICD
                                       PARA-KANRINO
                                       PARA-SAKUCD
                                       PARA-SYKYMD
                                       PARA-TENYMD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF1.
     MOVE      "NFJOHOF1"   TO   AB-FILE.
     MOVE      JH1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOF2.
     MOVE      "NFJOHOF2"   TO   AB-FILE.
     MOVE      JH2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFMEIMS.
     MOVE      "NFMEIMS"    TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SAKUBAF.
     MOVE      "SAKUBAF"    TO   AB-FILE.
     MOVE      SAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSERRF.
     MOVE      "NFSERRF"    TO   AB-FILE.
     MOVE      ERR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHAKOF.
     MOVE      "NFHAKOF"    TO   AB-FILE.
     MOVE      HAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEF.
     MOVE      "NFSUTEF"    TO   AB-FILE.
     MOVE      STE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =    "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     INITIALIZE                   WK-AREA.
     OPEN     I-O       NFJOHOF1  NFJOHOF2
     OPEN     INPUT     TENMS1    NFMEIMS    SAKUBAF.
     OPEN     I-O       NFHAKOF   NFSUTEF    NFSERRF.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*基本情報Ｆ初期読込
     IF   PARA-KBN   = "2"
          MOVE   SPACE               TO   JH1-REC
**********INITIALIZE                      JH1-REC
*
          MOVE   PARA-KANRINO        TO   JH1-F01
*         MOVE   PARA-SAKUCD         TO   JH1-F05
*         MOVE   00000               TO   JH1-F06
*         MOVE   000000000           TO   JH1-F07
*         MOVE   00                  TO   JH1-F08
*         MOVE   00000000            TO   JH1-F09
*
          START  NFJOHOF1  KEY IS >=   JH1-F01  JH1-F05
                                       JH1-F06  JH1-F07
                                       JH1-F08  JH1-F09
                 INVALID   KEY
                     MOVE     "END"      TO   END-FLG
          END-START
*
          IF       END-FLG   =   "END"
                   DISPLAY NC"＃対象データ無し１＃" UPON CONS
          ELSE
              PERFORM   NFJOHOF1-RD-SEC
              IF   END-FLG   =   "END"
                   DISPLAY NC"＃対象データ無し２＃" UPON CONS
              END-IF
          END-IF
     ELSE
          MOVE   SPACE               TO   JH2-REC
**********INITIALIZE                      JH2-REC
*
          MOVE   PARA-BACHI-YMD      TO   JH2-F02
          MOVE   PARA-BACHI-TIME     TO   JH2-F03
          MOVE   PARA-BACHI-TORICD   TO   JH2-F04
*         MOVE   PARA-SAKUCD         TO   JH2-F05
*         MOVE   00000               TO   JH2-F06
*         MOVE   000000000           TO   JH2-F07
*         MOVE   00                  TO   JH2-F08
*         MOVE   00000000            TO   JH2-F09
*
          START  NFJOHOF2  KEY  >=   JH2-F02  JH2-F03  JH2-F04
                                     JH2-F05  JH2-F06  JH2-F07
                                     JH2-F08  JH2-F09
                 INVALID   KEY
                 MOVE     "END"      TO   END-FLG
          END-START
*
          IF       END-FLG   =   "END"
                   DISPLAY NC"＃対象データ無し＃３＃" UPON CONS
          ELSE
              PERFORM   NFJOHOF2-RD-SEC
              IF   END-FLG   =   "END"
                   DISPLAY NC"＃対象データ無し＃４＃" UPON CONS
              END-IF
          END-IF
     END-IF.
*
     IF   END-FLG   NOT =  "END"
          MOVE      JH3-F01     TO    BRK-F01
          MOVE      JH3-F02     TO    BRK-F02
          MOVE      JH3-F03     TO    BRK-F03
          MOVE      JH3-F04     TO    BRK-F04
          MOVE      JH3-F05     TO    BRK-F05
          MOVE      JH3-F06     TO    BRK-F06
          MOVE      JH3-HE20    TO    BRK-HE20
          MOVE      JH3-F09     TO    BRK-F08
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　基本情報ファイル　管理_用　　
****************************************************************
 NFJOHOF1-RD-SEC            SECTION.
*
     MOVE    "NFJOHOF1-RD-SEC"    TO   S-NAME.
*
     READ     NFJOHOF1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFJOHOF1-RD-EXIT
     END-READ.
*
 NFJOHOF1-RD-001.
     IF   PARA-KANRINO  >  ZERO
          IF   JH1-F01   >   PARA-KANRINO
               MOVE   "END"            TO   END-FLG
               GO     TO    NFJOHOF1-RD-EXIT
          END-IF
     END-IF.
*
 NFJOHOF1-RD-002.
     IF   PARA-KBN      =  "2"
          IF   JH1-F17   NOT =   "2"
               GO     TO    NFJOHOF1-RD-SEC
          END-IF
     END-IF.
*
 NFJOHOF1-RD-003.
     IF   PARA-SAKUCD   NOT =  SPACE
          IF   JH1-F05    NOT =  PARA-SAKUCD
               GO     TO    NFJOHOF1-RD-SEC
          END-IF
     END-IF.
*
 NFJOHOF1-RD-004.
     IF   PARA-SYKYMD   NOT =  ZERO
     IF   PARA-SYKYMD   IS NUMERIC   AND
          PARA-SYKYMD       >  ZERO
          IF   JH1-F10    NOT =  PARA-SYKYMD
               GO     TO    NFJOHOF1-RD-SEC
          END-IF
     END-IF.
*
 NFJOHOF1-RD-005.
*****IF   PARA-TENYMD   NOT =  ZERO
     IF   PARA-TENYMD   IS NUMERIC  AND
          PARA-TENYMD       >  ZERO
          IF   JH1-F09    NOT =  PARA-TENYMD
               GO     TO    NFJOHOF1-RD-SEC
          END-IF
     END-IF.
*
     MOVE    JH1-REC            TO   JH3-REC.
     ADD     1                  TO   RD-CNT.
*
 NFJOHOF1-RD-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　基本情報ファイル　バッチＮＯ　
****************************************************************
 NFJOHOF2-RD-SEC            SECTION.
*
     MOVE    "NFJOHOF2-RD-SEC"    TO   S-NAME.
*
     READ     NFJOHOF2
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFJOHOF2-RD-EXIT
     END-READ.
*
 NFJOHOF2-RD-001.
     IF   PARA-BACHI-YMD      >  ZERO  AND
          PARA-BACHI-TIME     >  ZERO  AND
          PARA-BACHI-TORICD   >  ZERO
          IF   JH2-F02   >   PARA-BACHI-YMD      AND
               JH2-F03   >   PARA-BACHI-TIME     AND
               JH2-F04   >   PARA-BACHI-TORICD
               MOVE   "END"            TO   END-FLG
               GO     TO    NFJOHOF2-RD-EXIT
          END-IF
     END-IF.
*
     IF   PARA-KBN      =  "1"
          IF   JH2-F17   NOT =   "1"
               GO     TO    NFJOHOF2-RD-SEC
          END-IF
     END-IF.
*
*NFJOHOF2-RD-002.
*    IF   PARA-KBN      =  "2"
*         IF   JH2-F17   NOT =   "2"
*              GO     TO    NFJOHOF2-RD-SEC
*         END-IF
*    END-IF.
*
 NFJOHOF2-RD-002.
     IF   PARA-SAKUCD   NOT =  SPACE
          IF   JH2-F05    NOT =  PARA-SAKUCD
               GO     TO    NFJOHOF2-RD-SEC
          END-IF
     END-IF.
*
 NFJOHOF2-RD-003.
     IF   PARA-SYKYMD   NOT =  ZERO
          IF   JH2-F10    NOT =  PARA-SYKYMD
               GO     TO    NFJOHOF2-RD-SEC
          END-IF
     END-IF.
*
 NFJOHOF2-RD-004.
     IF   PARA-TENYMD   NOT =  ZERO
          IF   JH2-F09    NOT =  PARA-TENYMD
               GO     TO    NFJOHOF2-RD-SEC
          END-IF
     END-IF.
*
     MOVE    JH2-REC            TO   JH3-REC.
     ADD     1                  TO   RD-CNT.
*
 NFJOHOF2-RD-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*  チェックフラグクリア
     MOVE    SPACE               TO   CHK-FLG.
*
*　エラーチェック
     PERFORM  ERRCHK-SEC.
*
*  ファイル出力
     IF   CHK-FLG   =  SPACE
*      箱数ファイル出力
*
       IF    PARA-KBN   =   2
             IF  JH3-F01   NOT =   BRK-F01  OR
                 JH3-F05   NOT =   BRK-F05  OR
                 JH3-F06   NOT =   BRK-F06  OR
                 JH3-HE20  NOT =   BRK-HE20 OR
                 JH3-F09   NOT =   BRK-F08
*
                 IF   STE-O-CNT   >  ZERO   OR
                      STE-R-CNT   >  ZERO
                      PERFORM   NFHAKOF-WT-SEC
                 END-IF
*
                 MOVE      JH3-F01     TO    BRK-F01
                 MOVE      JH3-F02     TO    BRK-F02
                 MOVE      JH3-F03     TO    BRK-F03
                 MOVE      JH3-F04     TO    BRK-F04
                 MOVE      JH3-F05     TO    BRK-F05
                 MOVE      JH3-F06     TO    BRK-F06
                 MOVE      JH3-HE20    TO    BRK-HE20
                 MOVE      JH3-F09     TO    BRK-F08
                 MOVE      ZERO        TO    WK-GOKEI
                 INITIALIZE                  WK-GOKEI
             END-IF
       ELSE
             IF  JH3-F02   NOT =   BRK-F02  OR
                 JH3-F03   NOT =   BRK-F03  OR
                 JH3-F04   NOT =   BRK-F04  OR
                 JH3-F05   NOT =   BRK-F05  OR
                 JH3-F06   NOT =   BRK-F06  OR
                 JH3-HE20  NOT =   BRK-HE20 OR
                 JH3-F09   NOT =   BRK-F08
*
                 IF   STE-O-CNT   >  ZERO   OR
                      STE-R-CNT   >  ZERO
                      PERFORM   NFHAKOF-WT-SEC
                 END-IF
*
                 MOVE      JH3-F01     TO    BRK-F01
                 MOVE      JH3-F02     TO    BRK-F02
                 MOVE      JH3-F03     TO    BRK-F03
                 MOVE      JH3-F04     TO    BRK-F04
                 MOVE      JH3-F05     TO    BRK-F05
                 MOVE      JH3-F06     TO    BRK-F06
                 MOVE      JH3-HE20    TO    BRK-HE20
                 MOVE      JH3-F09     TO    BRK-F08
                 MOVE      ZERO        TO    WK-GOKEI
                 INITIALIZE                  WK-GOKEI
             END-IF
       END-IF
*
*      訂正数量ファイル
       ADD       JH3-F19         TO    WK-SURYO
*******COMPUTE   WK-HAKOSU  =    JH3-F19  /   JH3-F21
       MOVE      JH3-22          TO    WK-HAKOSU
       ADD       WK-HAKOSU       TO    WK-SOKONSU
*
       PERFORM   NFSUTEF-WT-SEC
*
*      基本情報ファイル出荷指示作成区分、日付更新
       PERFORM   NFJOHOF-WT-SEC
     ELSE
       PERFORM   NFSERRF-WT-SEC
     END-IF.
*
*基本情報Ｆ読込
     IF    PARA-KBN   = "2"
           PERFORM   NFJOHOF1-RD-SEC
     ELSE
           PERFORM   NFJOHOF2-RD-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　エラーチェック処理　　　　　　　　　　　　　　　*
****************************************************************
 ERRCHK-SEC   SECTION.
*
     MOVE    "ERRCHK-SEC"        TO   S-NAME.
*
     MOVE    SPACE               TO   WK-ERKBN.
*
*作場ＣＤ存在チェック
     MOVE  JH3-F05               TO      SAK-F01
     READ  SAKUBAF
           INVALID  KEY
               MOVE   "1"        TO      CHK-FLG
               MOVE   "1   "     TO      WK-ERKBN
               GO   TO   ERRCHK-EXIT
     END-READ.
*
*店ＣＤチェック
     MOVE  JH3-F04               TO      TEN-F52
     MOVE  JH3-F06               TO      TEN-F011
     READ  TENMS1
           INVALID  KEY
               MOVE   "1"        TO      CHK-FLG
               MOVE   "2   "     TO      WK-ERKBN
               GO   TO   ERRCHK-EXIT
     END-READ.
*
*商品ＣＤチェック
     MOVE  JH3-F13               TO      MEI-F01
     READ  NFMEIMS
           INVALID  KEY
               MOVE   "1"        TO      CHK-FLG
               MOVE   "3   "     TO      WK-ERKBN
               GO   TO   ERRCHK-EXIT
           NOT INVALID  KEY
*  ＪＡＮＣＤチェック
***************IF   JH3-F14   NOT =  MEI-F04
*                   MOVE   "1"        TO      CHK-FLG
*                   MOVE   "A   "     TO      WK-ERKBN
*                   GO   TO   ERRCHK-EXIT
***************END-IF
*  入数チェック
               IF   JH3-F18   NOT =  MEI-F09
                    MOVE   "1"        TO      CHK-FLG
                    MOVE   "5   "     TO      WK-ERKBN
                    GO   TO   ERRCHK-EXIT
               END-IF
*  原価単価チェック
************** COMPUTE  WK-GENKA  =   JH3-ME10  /  100
*              IF   WK-GENKA    NOT =  MEI-F11
*                   MOVE   "1"        TO      CHK-FLG
*                   MOVE   "8   "     TO      WK-ERKBN
*                   GO   TO   ERRCHK-EXIT
***************END-IF
*  売価単価チェック
***************COMPUTE  WK-BAIKA  =   JH3-ME12  /  100
*              IF   JH3-ME12  NOT =  MEI-F12
*                   MOVE   "1"        TO      CHK-FLG
*                   MOVE   "9   "     TO      WK-ERKBN
*                   GO   TO   ERRCHK-EXIT
***************END-IF
*  箱数チェック
               IF   JH3-F21  NOT =  MEI-F10
                    MOVE   "1"        TO      CHK-FLG
                    MOVE   "6   "     TO      WK-ERKBN
                    GO   TO   ERRCHK-EXIT
               END-IF
     END-READ.
*
 ERRCHK-EXIT.
     EXIT.
****************************************************************
*　　　　　　箱数ファイル出力                                  *
****************************************************************
 NFHAKOF-WT-SEC              SECTION.
*
     MOVE    "NFHAKOF-WT-SEC"   TO        S-NAME.
*
     MOVE     SPACE         TO        OUT-FLG.
     MOVE     SPACE         TO        HAK-REC.
     INITIALIZE                       HAK-REC.
* 箱数ファイル存在チェック.
     MOVE    BRK-F01        TO        HAK-F01.
     MOVE    BRK-F05        TO        HAK-F05.
     MOVE    BRK-F06        TO        HAK-F06.
     MOVE    BRK-HE20       TO        HAK-F07.
     MOVE    BRK-F08        TO        HAK-F08.
     READ   NFHAKOF
            INVALID KEY
              MOVE  "1"     TO        OUT-FLG
     END-READ.
*
     IF     OUT-FLG  =  "1"
            MOVE    BRK-F01       TO        HAK-F01
            MOVE    BRK-F02       TO        HAK-F02
            MOVE    BRK-F03       TO        HAK-F03
            MOVE    BRK-F04       TO        HAK-F04
            MOVE    BRK-F05       TO        HAK-F05
            MOVE    BRK-F06       TO        HAK-F06
            MOVE    BRK-HE20      TO        HAK-F07
            MOVE    BRK-F08       TO        HAK-F08
            MOVE    WK-SOKONSU    TO        HAK-F09
            MOVE    WK-SURYO      TO        HAK-F10
            MOVE    SPACE         TO        HAK-F98
            MOVE    ZERO          TO        HAK-F99
            WRITE   HAK-REC
            ADD     1             TO        HAK-O-CNT
     ELSE
********IF  HAK-F98    NOT =  SPACE
            MOVE    BRK-F01       TO        HAK-F01
            MOVE    BRK-F02       TO        HAK-F02
            MOVE    BRK-F03       TO        HAK-F03
            MOVE    BRK-F04       TO        HAK-F04
            MOVE    BRK-F05       TO        HAK-F05
            MOVE    BRK-F06       TO        HAK-F06
            MOVE    BRK-HE20      TO        HAK-F07
            MOVE    BRK-F08       TO        HAK-F08
            MOVE    WK-SOKONSU    TO        HAK-F09
            MOVE    WK-SURYO      TO        HAK-F10
            REWRITE   HAK-REC
            ADD     1             TO        HAK-R-CNT
******* ELSE
*****       MOVE    "7   "        TO        WK-ERKBN
*****       PERFORM   NFSERRF-WT-SEC
******* END-IF
     END-IF.
 NFHAKOF-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　数量訂正ファイル出力                              *
****************************************************************
 NFSUTEF-WT-SEC              SECTION.
*
     MOVE    "NFSUTEF-WT-SEC"   TO        S-NAME.
*
     MOVE     SPACE         TO        OUT-FLG.
     MOVE     SPACE         TO        STE-REC.
     INITIALIZE                       STE-REC.
* 箱数ファイル存在チェック.
     MOVE    JH3-F01        TO        STE-F01. *>管理番号
     MOVE    JH3-F05        TO        STE-F05. *>倉庫CD
     MOVE    JH3-F06        TO        STE-F06. *>店舗CD
     MOVE    JH3-HE20       TO        STE-F07. *>納品場所
     MOVE    JH3-F09        TO        STE-F08. *>店着日
     MOVE    JH3-HE04       TO        STE-F09. *>伝票番号
     DISPLAY "JH3-F01  = " JH3-F01    UPON CONS.
     DISPLAY "JH3-F05  = " JH3-F05    UPON CONS.
     DISPLAY "JH3-F06  = " JH3-F06    UPON CONS.
     DISPLAY "JH3-HE20 = " JH3-HE20   UPON CONS.
     DISPLAY "JH3-F09  = " JH3-F09    UPON CONS.
     DISPLAY "JH3-HE04 = " JH3-HE04   UPON CONS.
     READ   NFSUTEF
            INVALID KEY
              MOVE  "1"     TO        OUT-FLG
     END-READ.
*
     DISPLAY "OUT-FLG = " OUT-FLG UPON CONS.
     IF     OUT-FLG  =  "1"
            MOVE    JH3-F01       TO        STE-F01
            MOVE    JH3-F02       TO        STE-F02
            MOVE    JH3-F03       TO        STE-F03
            MOVE    JH3-F04       TO        STE-F04
            MOVE    JH3-F05       TO        STE-F05
            MOVE    JH3-F06       TO        STE-F06
            MOVE    JH3-HE20      TO        STE-F07
            MOVE    JH3-F09       TO        STE-F08
            MOVE    JH3-HE04      TO        STE-F09
            MOVE    JH3-F14       TO        STE-F10
            MOVE    JH3-F19       TO        STE-F11
            MOVE    JH3-F23       TO        STE-F12
            MOVE    JH3-F11       TO        STE-F13
            MOVE    JH3-F10       TO        STE-F14
            MOVE    JH3-F17       TO        STE-F15
            MOVE    SPACE         TO        STE-F98
            MOVE    ZERO          TO        STE-F99
            WRITE   STE-REC
            ADD     1             TO        STE-O-CNT
     ELSE
        IF          STE-F98    NOT =  SPACE
            MOVE    JH3-F01       TO        STE-F01
            MOVE    JH3-F02       TO        STE-F02
            MOVE    JH3-F03       TO        STE-F03
            MOVE    JH3-F04       TO        STE-F04
            MOVE    JH3-F05       TO        STE-F05
            MOVE    JH3-F06       TO        STE-F06
            MOVE    JH3-HE20      TO        STE-F07
            MOVE    JH3-F09       TO        STE-F08
            MOVE    JH3-HE04      TO        STE-F09
            MOVE    JH3-F14       TO        STE-F10
            MOVE    JH3-F19       TO        STE-F11
            MOVE    JH3-F23       TO        STE-F12
            MOVE    JH3-F11       TO        STE-F13
            MOVE    JH3-F10       TO        STE-F14
            MOVE    JH3-F17       TO        STE-F15
            REWRITE   STE-REC
            ADD     1             TO        STE-R-CNT
        ELSE
            MOVE    "7   "        TO        WK-ERKBN
            PERFORM   NFSERRF-WT-SEC
        END-IF
     END-IF.
 NFSUTEF-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　出荷情報エラーファイル                            *
****************************************************************
 NFSERRF-WT-SEC              SECTION.
*
     MOVE    "NFSERRF-WT-SEC"   TO        S-NAME.
*
     MOVE    SPACE          TO        ERR-REC.
     INITIALIZE                       ERR-REC.
*
* 項目転送
     MOVE    JH3-F01        TO        ERR-F01.
     MOVE    JH3-F02        TO        ERR-F02.
     MOVE    JH3-F03        TO        ERR-F03.
     MOVE    JH3-F04        TO        ERR-F04.
     MOVE    JH3-F05        TO        ERR-F05.
     MOVE    JH3-F06        TO        ERR-F06.
     MOVE    JH3-F10        TO        ERR-F07.
     MOVE    JH3-F09        TO        ERR-F08.
     MOVE    JH3-F13        TO        ERR-F09.
     MOVE    MEI-F05        TO        ERR-F10.
     MOVE    JH3-F18        TO        ERR-F11.
     MOVE    JH3-F19        TO        ERR-F12.
     MOVE    JH3-F21        TO        ERR-F13.
     MOVE    WK-ERKBN       TO        ERR-F14.
*
     WRITE   ERR-REC.
     ADD     1              TO        ERR-O-CNT.
*
 NFSERRF-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　出荷情報ファイル更新　                            *
****************************************************************
 NFJOHOF-WT-SEC              SECTION.
*
     MOVE    "NFJOHOF-WT-SEC"   TO        S-NAME.
*
* 項目転送
     IF   PARA-KBN  =  "2"
**********MOVE    "1"       TO        JH1-F24
**********MOVE    SYS-DATEW TO        JH1-F25
          MOVE    "1"       TO        JH1-F26
          MOVE    SYS-DATEW TO        JH1-F27
*
          REWRITE   JH1-REC
          ADD     1         TO        JH1-R-CNT
     ELSE
**********MOVE    "1"       TO        JH2-F24
**********MOVE    SYS-DATEW TO        JH2-F25
          MOVE    "1"       TO        JH2-F26
          MOVE    SYS-DATEW TO        JH2-F27
*
          REWRITE   JH2-REC
          ADD     1         TO        JH2-R-CNT
     END-IF.
*
 NFJOHOF-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     IF    STE-R-CNT  >  ZERO  OR
           STE-O-CNT  >  ZERO
           PERFORM   NFHAKOF-WT-SEC
     END-IF.
*
     MOVE      RD-CNT      TO      IN-CNT.
     MOVE      HAK-O-CNT   TO      OUT-CNT1.
     MOVE      HAK-R-CNT   TO      OUT-CNT11.
     MOVE      STE-O-CNT   TO      OUT-CNT2
     MOVE      STE-R-CNT   TO      OUT-CNT22
     MOVE      JH1-R-CNT   TO      OUT-CNT3
     MOVE      JH2-R-CNT   TO      OUT-CNT33
     MOVE      ERR-O-CNT   TO      OUT-CNT4
     DISPLAY   MSG-IN      UPON CONS.
     DISPLAY   MSG-OUT1    UPON CONS.
     DISPLAY   MSG-OUT11   UPON CONS.
     DISPLAY   MSG-OUT2    UPON CONS.
     DISPLAY   MSG-OUT22   UPON CONS.
     DISPLAY   MSG-OUT3    UPON CONS.
     DISPLAY   MSG-OUT33   UPON CONS.
     DISPLAY   MSG-OUT4    UPON CONS.
     DISPLAY   MSG-END     UPON CONS.
*
     CLOSE     NFJOHOF1  NFJOHOF2
     CLOSE     TENMS1    NFMEIMS    SAKUBAF.
     CLOSE     NFHAKOF   NFSUTEF    NFSERRF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
