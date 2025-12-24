****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　配信データ抽出　　　　　　　　　　*
*    作成日／更新日　　　：　92/12/05                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*    作成日／更新日　　　：　08/08/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　内部統制対応　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0130B.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          92/12/05.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SEIGKF2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01   SEI-F03
                                       SEI-F04   SEI-F05
                        STATUS         SEI-ST.
*----<< 請求合計ファイル２ >>--*
**** SELECT   HSEIGKF2  ASSIGN         DA-01-VI-HSEIGKF2
****                    ORGANIZATION   INDEXED
****                    ACCESS    MODE RANDOM
****                    RECORD    KEY  SE2-F01   SE2-F05
****                    STATUS         SE2-ST1   SE2-ST2.
*----<< 支払合計ファイル >>--*
**** SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
****                    ORGANIZATION   SEQUENTIAL
****                    STATUS         SHI-ST1   SHI-ST2.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< ワークファイル >>--*
     SELECT   WORK01    ASSIGN         DA-01-S-WORK01
                        ORGANIZATION   SEQUENTIAL
                        STATUS         WORK-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 請求合計ファイル２ >>--*
*FD  HSEIGKF2           LABEL RECORD   IS   STANDARD.
**** COPY     HSEIGKF2  OF        XFDLIB
****          JOINING   SE2       PREFIX.
*----<< 支払合計ファイル >>--*
*FD  HSHIGKF            LABEL RECORD   IS   STANDARD.
**** COPY     HSHIGKFR  OF        XFDLIB
****          JOINING   SHI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< ワークファイル >>--*
 FD  WORK01             LABEL RECORD   IS   STANDARD.
 01  WORK-REC           PIC  X(100).
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
 01  COUNTERS.
     03  ERR-CNT        PIC  9(06).
     03  SEI-CNT        PIC  9(06).
     03  SHI-CNT        PIC  9(06).
     03  SEO-CNT        PIC  9(06).
     03  SHO-CNT        PIC  9(06).
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEI-ST             PIC  X(02).
 01  SE2-ST             PIC  X(02).
 01  SHI-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  WORK-ST            PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  OLD-F02            PIC  9(06).
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
         05  FILLER     PIC  X(40)  VALUE  SPACE.
         05  FILLER     PIC  N(15)  VALUE
                        NC"【　抽出データエラーリスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(15)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"処理日".
         05  FILLER     PIC  X(01)  VALUE  ":".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"頁".
         05  FILLER     PIC  X(01)  VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)  VALUE  SPACE.
         05  FILLER     PIC  N(05)  VALUE  NC"取　引　先".
         05  FILLER     PIC  X(20)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"店　　舗".
         05  FILLER     PIC  X(21)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"検".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"収".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"日".
         05  FILLER     PIC  X(03)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"伝".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"票".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"_".
         05  FILLER     PIC  X(06)  VALUE  SPACE.
         05  FILLER     PIC  N(04)  VALUE  NC"請求金額".
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"納".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"品".
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  N(01)  VALUE  NC"日".
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"得意先".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10).
         05  ME-03      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-05      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  N(10).
         05  FILLER     PIC  X(03).
         05  ME-071     PIC  Z9.
         05  ME-071P    PIC  X(01).
         05  ME-072     PIC  Z9.
         05  ME-072P    PIC  X(01).
         05  ME-073     PIC  Z9.
         05  FILLER     PIC  X(03).
         05  ME-08      PIC  X(09).
         05  FILLER     PIC  X(01).
         05  ME-10      PIC  ----,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-111     PIC  Z9.
         05  ME-111P    PIC  X(01).
         05  ME-112     PIC  Z9.
         05  ME-112P    PIC  X(01).
         05  ME-113     PIC  Z9.
         05  FILLER     PIC  X(02).
         05  ME-12      PIC  9(08).
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
 LINKAGE                SECTION.
 01  PARA-OUT-CNT       PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 請求合計ファイル >>--*
 HSEIGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0130B HSEIGKF ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
***  CLOSE    HSEIGKF   HSHIGKF   HTOKMS    HTENMS  WORK01  PRTF.
     STOP     RUN.
*----<< 請求合計ファイル２ >>--*
*HSEIGKF-ERR             SECTION.
*    USE AFTER     EXCEPTION PROCEDURE      HSEIGKF2.
*    ACCEPT   SYS-DATE       FROM DATE.
*    ACCEPT   SYS-TIME       FROM TIME.
*    DISPLAY  "### SSE0130B HSEIGKF2 ERROR " SE2-ST " "
*             SYS-YY "." SYS-MM "." SYS-DD " "
**            SYS-HH ":" SYS-MN ":" SYS-SS " ###"
*                                      UPON CONS.
***  STOP     RUN.
*----<< 支払合計ファイル >>--*
*SHI-ERR                SECTION.
*    USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
*    ACCEPT   SYS-DATE       FROM DATE.
*    ACCEPT   SYS-TIME       FROM TIME.
*    DISPLAY  "### SSE0130B HSHIGKF ERROR " SHI-ST " "
*             SYS-YY "." SYS-MM "." SYS-DD " "
**            SYS-HH ":" SYS-MN ":" SYS-SS " ###"
**                                     UPON CONS.
**** CLOSE    HSEIGKF   HSHIGKF   HTOKMS    HTENMS  WORK01  PRTF.
***  STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0130B HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
***  CLOSE    HSEIGKF   HSHIGKF   HTOKMS    HTENMS  WORK01  PRTF.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0130B HTENMS ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
***  CLOSE    HSEIGKF   HSHIGKF   HTOKMS    HTENMS  WORK01  PRTF.
     STOP     RUN.
*----<< ワークファイル >>--*
 WORK01-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      WORK01.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0130B WORK01 ERROR " WORK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
***  CLOSE    HSEIGKF   HSHIGKF   HTOKMS    HTENMS  WORK01  PRTF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0130B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HSEIGKF.
**** OPEN     INPUT     HSEIGKF2.
**** OPEN     INPUT     HSHIGKF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    WORK01.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE     0         TO   END-FLG.
     INITIALIZE    SEI-REC.
     PERFORM  900-SEI-READ.
     PERFORM  210-SEI-OUT    UNTIL     END-FLG   =    1.
*
**** MOVE     SEI-F02   TO   OLD-F02.
**** MOVE     0         TO   END-FLG.
**** PERFORM  900-SHI-READ.
**** PERFORM  220-SHI-OUT    UNTIL     END-FLG   =    1.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HSEIGKF.
**** CLOSE    HSEIGKF2.
**** CLOSE    HSHIGKF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    WORK01.
     CLOSE    PRTF.
*
     DISPLAY "+++ ｾｲｷｭｳ INPUT =" SEI-CNT " +++" UPON CONS.
     DISPLAY "+++ ｾｲｷｭｳ OUTPUT=" SEO-CNT " +++" UPON CONS.
*****内部統制対応 2008/08/28
     MOVE     SEO-CNT   TO       PARA-OUT-CNT.
**** DISPLAY "+++ ｼﾊﾗｲ  INPUT =" SHI-CNT " +++" UPON CONS.
**** DISPLAY "+++ ｼﾊﾗｲ  OUTPUT=" SHO-CNT " +++" UPON CONS.
     DISPLAY "+++ ｴﾗｰ   ｹﾝｽｳ  =" ERR-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0130B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｾｲｷｭｳ ﾃﾞｰﾀ ｼｮﾘ                              *
*--------------------------------------------------------------*
 210-SEI-OUT            SECTION.
     IF       SEI-F08   =    9
**** OR  ( (  SEI-F07   =    41   OR   42   OR   44   )
****     AND  SEI-F05(1:2)   NOT  =    "07"  )
     OR  ( (  SEI-F06   <    ZERO )
         AND  SEI-F05(1:2)   NOT  =    "07"  )
              ADD       1         TO   ERR-CNT
              PERFORM   211-ERR-PRINT
     ELSE
              ADD       1         TO   SEO-CNT
              WRITE     WORK-REC  FROM SEI-REC
     END-IF.
*
     PERFORM  900-SEI-READ.
 210-SEI-OUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｼﾊﾗｲ  ﾃﾞｰﾀ ｼｮﾘ                              *
*--------------------------------------------------------------*
*220-SHI-OUT            SECTION.
*    IF       SHI-F18   =    "8"
*             ADD       1         TO   SHO-CNT
*             MOVE      SPACE     TO   SEI-REC
*             INITIALIZE               SEI-REC
****          MOVE      SHI-F09   TO   SEI-F01
*             MOVE      OLD-F02   TO   SEI-F02
*             MOVE      SHI-F19   TO   SEI-F03
*             MOVE      SHI-F20   TO   SEI-F04
*             MOVE      SHI-F21   TO   SEI-F05
****          MOVE      SHI-F22   TO   SEI-F06
*
*             MOVE      173       TO   SE2-F01
*             MOVE      SHI-F21   TO   SE2-F05
*             READ      HSEIGKF2
*                  INVALID
*                       MOVE      SHI-F09   TO   SEI-F01
*                       MOVE      OLD-F02   TO   SEI-F02
*                       MOVE      SHI-F19   TO   SEI-F03
*                       MOVE      SHI-F20   TO   SEI-F04
*                       MOVE      SHI-F21   TO   SEI-F05
*                       MOVE      SHI-F22   TO   SEI-F06
*                  NOT INVALID
*                       MOVE      SE2-REC   TO   SEI-REC
*                       MOVE      OLD-F02   TO   SEI-F02
*             END-READ
*
*             WRITE     WORK-REC  FROM SEI-REC
*    END-IF.
*
*    PERFORM  900-SHI-READ.
*220-SHI-OUT-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 211-ERR-PRINT          SECTION.
     IF       LINE-CNT  >    63
              PERFORM   212-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     SEI-F01        TO   ME-03.
     MOVE     SEI-F01        TO   TOK-F01.
*****MOVE     173            TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-04.
     MOVE     SEI-F03        TO   ME-05.
     MOVE     SEI-F01        TO   TEN-F52.
*****MOVE     173            TO   TEN-F52.
     MOVE     SEI-F03        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   ME-06.
     MOVE     SEI-F04 (1:2)  TO   ME-071.
     MOVE     "."            TO   ME-071P.
     MOVE     SEI-F04 (3:2)  TO   ME-072.
     MOVE     "."            TO   ME-072P.
     MOVE     SEI-F04 (5:2)  TO   ME-073.
     MOVE     SEI-F05        TO   ME-08.
     MOVE     SEI-F06        TO   ME-10.
     MOVE     SEI-F10 (1:2)  TO   ME-111.
     MOVE     "."            TO   ME-111P.
     MOVE     SEI-F10 (3:2)  TO   ME-112.
     MOVE     "."            TO   ME-112P.
     MOVE     SEI-F10 (5:2)  TO   ME-113.
     MOVE     SEI-F11        TO   ME-12.
*
     WRITE    PRT-REC   FROM MEIS01    AFTER     1.
     ADD      1         TO   LINE-CNT.
 211-ERR-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 212-HEAD-PRINT         SECTION.
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
     WRITE    PRT-REC   FROM      HEAD02    AFTER     3.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     7              TO   LINE-CNT.
 212-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
*900-SHI-READ           SECTION.
*    READ     HSHIGKF   AT   END
*             MOVE      1         TO   END-FLG
*             GO   TO   900-SHI-READ-EXIT
*    END-READ.
*
*    ADD      1              TO   SHI-CNT.
*900-SHI-READ-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     READ     HSEIGKF   AT   END
              MOVE      1         TO   END-FLG
              GO   TO   900-SEI-READ-EXIT
     END-READ.
     IF       SEI-F01   NOT  =    173  AND 3022
              GO   TO   900-SEI-READ
     END-IF.
*
     ADD      1              TO   SEI-CNT.
 900-SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
