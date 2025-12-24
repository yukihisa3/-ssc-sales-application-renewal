****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　片倉工業支払照合　　　　　　　　　*
*    モジュール名　　　　：　不照合リスト作成　　　　　　　　　*
*    作成日／更新日　　　：　95/11/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルと請求合計ファイル*
*                        ：　を比較して不照合リストを作成する。*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0080L.
 AUTHOR.                N.T
 DATE-WRITTEN.          95/11/15.
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
*----<< 支払合計ファイル >>--*
     SELECT   HSHIGKF   ASSIGN         DA-01-S-HSHIGKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SHI-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SEIGKF21
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F05
                        STATUS         SEI-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  HSHIGKF            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
     COPY     SITGKFC   OF        XFDLIB
              JOINING   SHI       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
     03  OUT-FLG        PIC  9(01).
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  IN-CNT         PIC  9(09).
*
     03  CNT-7          PIC  9(06).
     03  CNT-8          PIC  9(06).
     03  CNT-9          PIC  9(06).
     03  CNT-UNMACTH    PIC  9(06).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHI-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
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
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  OLD.
         05  OLD-18     PIC  X(01).
         05  OLD-09     PIC  X(04).
         05  OLD-19     PIC  X(05).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(34)     VALUE  SPACE.
         05  FILLER     PIC  N(10)     VALUE
                        NC"【　不照合リスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(29)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
******** 05  FILLER     PIC  N(06)     VALUE  NC"レコード区分".
         05  FILLER     PIC  N(06)     VALUE  NC"　　　　　　".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店　舗".
         05  FILLER     PIC  X(16)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(06)     VALUE  NC"検　収　日".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"請求金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(08) VALUE  NC"レコード区分内容".
         05  FILLER     PIC  X(04) VALUE  SPACE.
         05  FILLER     PIC  N(08) VALUE  NC"量販伝票_内容".
         05  FILLER     PIC  X(04) VALUE  SPACE.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(05) VALUE  NC"データ内容".
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(05).
         05  ME-03      PIC  9(01).
         05  FILLER     PIC  X(05).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-05      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-071     PIC  Z9.
         05  ME-071P    PIC  X(01).
         05  ME-072     PIC  Z9.
         05  ME-072P    PIC  X(01).
         05  ME-073     PIC  Z9.
         05  FILLER     PIC  X(02).
         05  ME-08      PIC  X(09).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  ----,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-10      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  N(10).
 01  MEIS02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(90)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"相殺伝票用".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  ME-13      PIC  N(16).
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
 01  TAIL01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"区分ー７".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA-01      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"区分ー８".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA-02      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"区分ー９".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA-03      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(13)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"総件数".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TA-04      PIC  ZZZ,ZZ9.
 01  TAIL02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(30)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
                        NC"　　　　　　　".
*********05  TA-05      PIC  ZZZ,ZZ9.
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  X(17)     VALUE  SPACE.
         05  FILLER     PIC  N(07)     VALUE
                        NC"金額不一致件数".
         05  TA-06      PIC  ZZZ,ZZ9.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0080L HSHIGKF ERROR " SHI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HSEIGKF    HTENMS    PRTF.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 HSEIGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0080L HSEIGKF ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HSEIGKF    HTENMS    PRTF.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSI0080L HTENMS ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HSHIGKF   HSEIGKF    HTENMS    PRTF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    1.
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
     DISPLAY  "*** SSI0080L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HSHIGKF.
     OPEN     INPUT     HSEIGKF.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     INITIALIZE         BREAK-KEY.
*
     PERFORM  900-SHI-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     IF       LINE-CNT  >    63
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     IF       LINE-CNT  =    6
     OR       SHI-F18   NOT  =    OLD-18
*==* OR       SHI-F09   NOT  =    OLD-09
     OR       SHI-F32   NOT  =    OLD-09
     OR       SHI-F19   NOT  =    OLD-19
*--*          MOVE      SHI-F18        TO   ME-03
*==*          MOVE      SHI-F09        TO   ME-04
              MOVE      SHI-F32        TO   ME-04
              MOVE      SHI-F19        TO   ME-05
*==*          MOVE      SHI-F09        TO   TEN-F52
              MOVE      SHI-F32        TO   TEN-F52
              MOVE      SHI-F19        TO   TEN-F011
              PERFORM   900-TEN-READ
              MOVE      TEN-F03        TO   ME-06
     END-IF.
     MOVE     SHI-F20        TO   WK-DATE.
     MOVE     WK-YY          TO   ME-071.
     MOVE     "."            TO   ME-071P.
     MOVE     WK-MM          TO   ME-072.
     MOVE     "."            TO   ME-072P.
     MOVE     WK-DD          TO   ME-073.
     MOVE     SHI-F21        TO   ME-08.
     MOVE     SHI-F22        TO   ME-09.
     EVALUATE SHI-F18
         WHEN "7"
              MOVE NC"ＯＫデータ"           TO   ME-10
         WHEN "8"
              MOVE NC"アンマッチ請求"       TO   ME-10
         WHEN "9"
              MOVE NC"返品相殺"             TO   ME-10
     END-EVALUATE.
*==* MOVE     SHI-F09        TO   SEI-F01.
     MOVE     SHI-F32        TO   SEI-F01.
     MOVE     "000"          TO   SEI-F05(1:3).
     MOVE     SHI-F21(1:6)   TO   SEI-F05(4:6).
     PERFORM  900-SEI-READ.
     EVALUATE INV-FLG
         WHEN 0
              MOVE NC"データが存在する"     TO   ME-11
              IF   SHI-F22   =    SEI-F06
                   IF   SHI-F18   =    7
                        GO   TO   200-MAIN-900
                   END-IF
                   MOVE  1                       TO   OUT-FLG
                   MOVE NC"金額が一致する"       TO   ME-12
              ELSE
                   MOVE  ZERO                    TO   OUT-FLG
                   MOVE NC"金額が一致しない"     TO   ME-12
                   ADD  1         TO   CNT-UNMACTH
              END-IF
         WHEN 1
              MOVE NC"データが存在しない"   TO   ME-11
     END-EVALUATE.
*
     IF     OUT-FLG  =  ZERO
            WRITE  PRT-REC  FROM MEIS01  AFTER  1
            ADD    1        TO           LINE-CNT
     ELSE
            MOVE   ZERO     TO           OUT-FLG
     END-IF.
*
     MOVE     SHI-F18        TO   OLD-18.
*==* MOVE     SHI-F09        TO   OLD-09.
     MOVE     SHI-F32        TO   OLD-09.
     MOVE     SHI-F19        TO   OLD-19.
*
 200-MAIN-900.
     PERFORM  900-SHI-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*    テイル印刷
     PERFORM  TAIL-RTN.
*
     CLOSE    HSHIGKF.
     CLOSE    HSEIGKF.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSI0080L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 210-HEAD-PRINT         SECTION.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
     END-IF.
*
     ADD      1              TO   PAGE-CNT.
     MOVE     SYS-YY         TO   HD-011.
     MOVE     SYS-MM         TO   HD-012.
     MOVE     SYS-DD         TO   HD-013.
     MOVE     PAGE-CNT       TO   HD-02.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     6              TO   LINE-CNT.
 210-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       テイル部印刷                                *
*--------------------------------------------------------------*
 TAIL-RTN               SECTION.
     IF       LINE-CNT  >    63
              PERFORM   210-HEAD-PRINT
     END-IF.
*
*    MOVE     IN-CNT         TO   TA-05.
     MOVE     CNT-UNMACTH    TO   TA-06.
*
*    WRITE    PRT-REC   FROM      TAIL01    AFTER     2.
     WRITE    PRT-REC   FROM      TAIL02    AFTER     2.
 TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HSEIGKF   INVALID
              MOVE      1         TO   INV-FLG
     END-READ.
 900-SEI-READ-EXIT.
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
 900-SHI-READ           SECTION.
     READ     HSHIGKF
         AT END
              MOVE      1         TO   END-FLG
         NOT AT END
              ADD       1         TO   IN-CNT
     END-READ.
*
 900-SHI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
