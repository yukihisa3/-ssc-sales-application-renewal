****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　トークオンライン　　　　　　　　　*
*    モジュール名　　　　：　請求データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　95/07/20                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより　　　　　　　*
*　　　　　　　　　　　　：　ＣＶＣＳ送信用ファイルを作成する　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSE0150B.
 AUTHOR.                TOMIZAWA.
 DATE-WRITTEN.          95/07/01.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル>>-*
     SELECT   SEIGKF    ASSIGN         DA-01-S-HSEIGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEI-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 配信データ >>--*
     SELECT   CVCSF     ASSIGN         DA-01-S-CVCSF
                        ORGANIZATION   SEQUENTIAL
                        STATUS    CVCS-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル>>-*
 FD  SEIGKF             LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 配信データ >>--*
 FD  CVCSF              LABEL     RECORD   IS   STANDARD.
 01  ONL-REC.
*ファイルヘッダ
     03  ONL0-REC.
         05  ONL00-A1             PIC  X(03).
         05  ONL00-A2             PIC  X(04).
         05  FILLER               PIC  X(83).
         05  ONL00-A3             PIC  X(08).
         05  ONL00-A4             PIC  9(06).
         05  ONL00-A5             PIC  9(08).
         05  FILLER               PIC  X(144).
     03  ONL1-REC   REDEFINES     ONL0-REC.
         05  ONL1-C1              PIC  X(03).
         05  ONL1-C2              PIC  X(04).
         05  ONL1-C3              PIC  9(02).
         05  FILLER               PIC  X(19).
         05  ONL1-C00             PIC  X(224).
         05  ONL1-C01             REDEFINES    ONL1-C00.
             07  ONL1-C02         OCCURS       7.
                 09  ONL1-C4      PIC  9(03).
                 09  ONL1-C5      PIC  9(06).
                 09  ONL1-C6      PIC  9(09).
                 09  ONL1-C7      PIC  X(01).
                 09  ONL1-C8      PIC  9(10).
                 09  FILLER       PIC  X(03).
     03  ONL2-REC   REDEFINES     ONL0-REC.
         05  ONL2-E1              PIC  X(03).
         05  ONL2-E2              PIC  X(04).
         05  FILLER               PIC  X(09).
         05  ONL2-E3              PIC  9(06).
         05  FILLER               PIC  X(68).
         05  ONL2-E4              PIC  X(08).
         05  ONL2-E5              PIC  9(06).
         05  ONL2-E6              PIC  9(08).
         05  FILLER               PIC  X(114).
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  GYO-CNT        PIC  9(02).
 01  IDX.
     03  I              PIC  9(03).
     03  J              PIC  9(03).
*
 01  WK-ATESAKI.
     03  WK-ATESAKI1    PIC  X(06).
     03  WK-ATESAKI2    PIC  X(02)    VALUE "01".
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TOK-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  CVCS-ST            PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN        PIC S9(11)     PACKED-DECIMAL.
     03  KEI-KIN        PIC S9(11)     PACKED-DECIMAL.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(47)  VALUE  SPACE.
         05  FILLER     PIC  N(12)  VALUE
                        NC"【　配信データリスト　】".
         05  FILLER     PIC  X(27)  VALUE  SPACE.
         05  FILLER     PIC  X(05)  VALUE  "DATE:".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-013     PIC  Z9.
 01  HEAD02.
         05  FILLER     PIC  X(110) VALUE  SPACE.
         05  FILLER     PIC  X(05)  VALUE  "PAGE:".
         05  HD-02      PIC  ZZZ9.
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"配信先".
         05  FILLER     PIC  X(35)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"伝票枚数".
         05  FILLER     PIC  X(08)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"請求金額".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(09).
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  N(16).
         05  FILLER     PIC  X(02).
         05  ME-05      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(02).
         05  ME-06      PIC  --,---,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ワークファイル >>--*
 SEIGKF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### VDA0900 SEIGKF ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### VDA0900 HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 配信データ >>--*
 CVCSKHAI-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CVCSF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### VDA0900 CVCSF    ERROR " CVCS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     IF       END-FLG   =    0
              PERFORM       MAIN-RTN
                            UNTIL END-FLG = 1
     END-IF.
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0150B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SEIGKF.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    CVCSF.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         IDX.
     INITIALIZE         GOKEI-AREA.
     MOVE     99             TO   LINE-CNT.
*
     PERFORM  SEI-READ.
*    ファイルヘッダ作成　
     MOVE     SPACE     TO        ONL0-REC.
     INITIALIZE                   ONL0-REC.
*
     MOVE     "56A"     TO        ONL00-A1.
     MOVE     "0928"    TO        ONL00-A2.
     MOVE     "KCS-VAN" TO        ONL00-A3.
     ACCEPT   ONL00-A4   FROM     DATE.
     ACCEPT   ONL00-A5   FROM     TIME.
     WRITE                        ONL-REC.
*
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*明細レコード
     MOVE     SPACE          TO   ONL1-REC.
     INITIALIZE                   ONL1-REC.
     MOVE     "56C"          TO   ONL1-C1.
     MOVE     "0928"         TO   ONL1-C2.
     MOVE     "01"           TO   ONL1-C3.
     MOVE     ZERO           TO   GYO-CNT.
*
     MOVE     SPACE          TO   ONL1-C00.
     PERFORM  VARYING        I    FROM 1 BY 1
              UNTIL     (    I    >    7       )
                     OR (    END-FLG   =    1  )
*
              PERFORM   EDIT-RTN
              COMPUTE   KEI-KIN   =    KEI-KIN  +
                                       SEI-F06
              PERFORM   SEI-READ
*
     END-PERFORM.
*
     ADD      GYO-CNT        TO   KEI-KEN.
*
     ADD      1              TO   OUT-CNT.
     WRITE    ONL-REC.
*
     IF       END-FLG        =    1
         IF   KEI-KEN  NOT   =        ZERO
              MOVE     SPACE      TO  ONL-REC
              INITIALIZE              ONL-REC
              MOVE    "56E"       TO  ONL2-E1
              MOVE    "0928"      TO  ONL2-E2
              MOVE    OUT-CNT     TO  ONL2-E3
              MOVE    "KCS-VAN"   TO  ONL2-E4
              ACCEPT  ONL2-E5     FROM DATE
              ACCEPT  ONL2-E6     FROM TIME
              WRITE               ONL-REC
*
         END-IF
     END-IF.
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　編集処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-RTN               SECTION.
*
     MOVE      SEI-F03       TO   ONL1-C4(I).
     MOVE      SEI-F04       TO   ONL1-C5(I).
     MOVE      SEI-F05       TO   ONL1-C6(I).
     EVALUATE  SEI-F07
        WHEN   40
         MOVE  ZERO          TO   ONL1-C7(I)
        WHEN   OTHER
         MOVE  1             TO   ONL1-C7(I)
     END-EVALUATE.
     MOVE      SEI-F06       TO   ONL1-C8(I).
*
     ADD       1             TO   GYO-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
*---<< ｺﾞｳｹｲ ｲﾝｻﾂ >>---*
     PERFORM  PRINT-RTN.
*
     CLOSE    SEIGKF.
     CLOSE    HTOKMS.
     CLOSE    CVCSF.
     CLOSE    PRTF.
*
     DISPLAY "+++ VDA0900 INPUT =" IN-CNT  " +++" UPON CONS.
     DISPLAY "+++ VDA0900 OUTPUT=" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0150B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　印刷処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PRINT-RTN              SECTION.
     IF       LINE-CNT  >    63
              PERFORM   HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     "00928"        TO   ME-03.
     MOVE     "00928"        TO   TOK-F01.
     PERFORM  TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
     MOVE     KEI-KEN        TO   ME-05.
     MOVE     KEI-KIN        TO   ME-06.
*
     WRITE    PRT-REC   FROM MEIS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
 PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　タイトルプリント　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HEAD-PRINT             SECTION.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1              TO   PAGE-CNT.
     MOVE     SYS-YY         TO   HD-011.
     MOVE     SYS-MM         TO   HD-012.
     MOVE     SYS-DD         TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     2.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     7              TO   LINE-CNT.
 HEAD-PRINT-EXIT.
     EXIT.
****************************************************************
*　　　　取引先マスタＲＥＡＤ　　　　　　　　　　　　　　　　　*
****************************************************************
 TOK-READ               SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
     READ     SEIGKF    AT   END
              MOVE      1         TO   END-FLG
              GO   TO   SEI-READ-EXIT
     END-READ.
*
     IF       SEI-F01   NOT =     928
              GO TO     SEI-READ
     END-IF.
*
*
     ADD      1              TO   IN-CNT.
 SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
