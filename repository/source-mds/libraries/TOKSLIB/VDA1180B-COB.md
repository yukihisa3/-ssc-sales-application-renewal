# VDA1180B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/VDA1180B.COB`

## ソースコード

```cobol
****************************************************************
*         SYSTEM･･･････サカタのタネ特販システム                *
*         PROGRAM-NAME･ＣＶＣＳ受信状況チェックリスト          *
*         PROGRAM-ID･･･VDA1180B                                *
*                                                              *
*         ユニディー検収データファイル件数リスト               *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            VDA1180B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          97/10/02.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.         CONSOLE   IS   CONS
                        YA        IS   YA
                        YB-21     IS   YB-21.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*
     SELECT   YNKENSYU  ASSIGN    TO        YNKENSYU
                        ACCESS    MODE IS   SEQUENTIAL.
*
     SELECT   PRINTF    ASSIGN    TO        LP-04.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    仕入伝票データ　ＲＬ＝２００　ＢＦ＝　５
******************************************************************
 FD  YNKENSYU
                        BLOCK CONTAINS      5    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  YUN-REC.
     03  YUN-01                   PIC  X(200).
*
 FD  PRINTF
                        LABEL RECORD   IS   OMITTED
                        LINAGE IS      66   LINES
                        DATA  RECORD   IS   PRINT-REC.
 01  PRINT-REC                    PIC       X(200).
*
*
 WORKING-STORAGE        SECTION.
*    エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE SPACE.
*
*    日付保存ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06).
     03  SYSYMD-R                 REDEFINES SYSYMD.
         05  SYS-YY               PIC       9(02).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
*    キーブレイク項目
 01  OLD-DENNO                         PIC  9(07)  VALUE  ZERO.
 01  NEW-DENNO                         PIC  9(07)  VALUE  ZERO.
*    伝票枚数
 01  DEN-CNT                           PIC  9(07)  VALUE  ZERO.
 01  GYO-CNT                           PIC  9(07)  VALUE  ZERO.
*
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(40)  VALUE SPACE.
     03  FILLER                   PIC       N(19)  VALUE
       NC"【検収データ件数リスト（ユニディー）】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(19)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "DATE:".
     03  YY                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  MM                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  DD                       PIC       Z9.
     03  FILLER                   PIC       X(13)  VALUE SPACE.
*    明細行
 01  MEISAI1            CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(08)  VALUE
       NC"データ件数　＝　".
     03  MEI1-MAISU               PIC       ZZZZZZ9.
*
 01  MEISAI2            CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(08)  VALUE
       NC"データ件数　＝　".
     03  FILLER                   PIC       N(11)  VALUE
       NC"検収データは０件です．".
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
 PROCESS                SECTION.

     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL    END-FLG = 1.
     PERFORM      END-SEC.
     STOP  RUN.
*
 PROCESS-END.
     EXIT.
******************************************************************
*                 INIT-SEC
******************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     YNKENSYU.
     OPEN     OUTPUT    PRINTF.
*
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE     ZERO      TO        GYO-CNT.
*
 INIT-END.
     EXIT.
******************************************************************
*                      MAIN-SEC
******************************************************************
 MAIN-SEC               SECTION.
*
     READ     YNKENSYU
           AT END
              MOVE    1     TO    END-FLG
           NOT AT END
              ADD     1     TO    GYO-CNT
     END-READ.
*
 MAIN-END.
     EXIT.
******************************************************************
*                      END-SEC
******************************************************************
 END-SEC                SECTION.
*
     MOVE     GYO-CNT        TO        MEI1-MAISU.
     MOVE     SYS-YY         TO        YY.
     MOVE     SYS-MM         TO        MM.
     MOVE     SYS-DD         TO        DD.
*
     WRITE    PRINT-REC      FROM      MIDASHI1 AFTER 2.
     IF       GYO-CNT  NOT =  ZERO
              WRITE  PRINT-REC  FROM  MEISAI1  AFTER 10
     END-IF.
     IF       GYO-CNT  =  ZERO
              WRITE  PRINT-REC  FROM  MEISAI2  AFTER 10
     END-IF.
*
     CLOSE    YNKENSYU  PRINTF.
********************<<  PROGRAM  END  >>**************************

```
