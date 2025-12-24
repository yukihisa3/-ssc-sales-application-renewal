# SEKCNT1

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SEKCNT1.COB`

## ソースコード

```cobol
****************************************************************
*         SYSTEM･･･････サカタのタネ特販システム                *
*         PROGRAM-NAME･ＣＶＣＳ受信状況チェックリスト          *
*         PROGRAM-ID･･･ＳＥＫＣＮＴ　                          *
*         1996.03.11　伝票データの他に検収データ，支払データ， *
*                     商品データの件数も表示する．             *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SEKCNT1.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2000/01/03.
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
     SELECT   KENSYUD   ASSIGN    TO        SKKENSYU
                        ACCESS    MODE IS   SEQUENTIAL.
*
     SELECT   SIHARAD   ASSIGN    TO        SKSIHARA
                        ACCESS    MODE IS   SEQUENTIAL.
*
     SELECT   SHOHIND   ASSIGN    TO        SKSHOHIN
                        ACCESS    MODE IS   SEQUENTIAL.
*
     SELECT   PRINTF    ASSIGN    TO        LP-04.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*----------------------------------------------------------------*
*    FILE = 検収情報データ        RL=300    BF=3      ORG=SF
*----------------------------------------------------------------*
 FD  KENSYUD
                        BLOCK CONTAINS      3    RECORDS
                        LABEL RECORD   IS   STANDARD
                        DATA  RECORD   IS   SKENREC.
 01  SKENREC.
*
     03  SKEN-REC01               PIC  X(128).
     03  SKEN-REC02               PIC  X(128).
     03  SKEN-REC03.
         05  SKEN-REC031          PIC  X(034).
         05  SKEN-REC032          PIC  9(005).
         05  SKEN-REC034          PIC  9(005).
*----------------------------------------------------------------*
*    FILE = 支払情報データ        RL=256    BF=4      ORG=SF
*----------------------------------------------------------------*
 FD  SIHARAD
                        BLOCK CONTAINS      31   RECORDS
                        LABEL RECORD   IS   STANDARD
                        DATA  RECORD   IS   SSIHAREC.
 01  SSIHAREC.
*
     03  SSIHA-REC01              PIC  X(128).
*****03  SSIHA-REC02              PIC  X(063).
*****03  SSIHA-REC03              PIC  X(065).
*----------------------------------------------------------------*
*    FILE = 商品情報データ        RL=256    BF=4      ORG=SF
*----------------------------------------------------------------*
 FD  SHOHIND
                        BLOCK CONTAINS      4    RECORDS
                        LABEL RECORD   IS   STANDARD
                        DATA  RECORD   IS   SSHOREC.
 01  SSHOREC.
*
     03  SSHO-REC01               PIC  X(128).
     03  SSHO-REC02               PIC  X(128).
*
 FD  PRINTF
                        LABEL RECORD   IS   OMITTED
                        LINAGE IS      66   LINES
                        DATA  RECORD   IS   PRINT-REC.
 01  PRINT-REC                    PIC       X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
 01  END-FLG                      PIC       9(01)  VALUE ZERO.
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
 01  OLD-DENNO                         PIC  9(09)  VALUE  ZERO.
 01  NEW-DENNO                         PIC  9(09)  VALUE  ZERO.
*    伝票枚数
 01  DEN-CNT                           PIC  9(07)  VALUE  ZERO.
 01  GYO-CNT                           PIC  9(07)  VALUE  ZERO.
 01  DEN-CNT1                          PIC  9(07)  VALUE  ZERO.
 01  GYO-CNT1                          PIC  9(07)  VALUE  ZERO.
 01  DEN-CNT2                          PIC  9(07)  VALUE  ZERO.
 01  GYO-CNT2                          PIC  9(07)  VALUE  ZERO.
 01  DEN-CNT3                          PIC  9(07)  VALUE  ZERO.
 01  GYO-CNT3                          PIC  9(07)  VALUE  ZERO.
*
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(22)  VALUE
       NC"【　オンライン件数リスト　（セキチュー　）】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(21)  VALUE SPACE.
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
*
*    明細行
 01  MEISAI1            CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(15)  VALUE
       NC"　　　　　　仕入伝票枚数　＝　".
     03  MEI1-MAISU               PIC       ZZZZZZ9.
*
 01  MEISAI2            CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(15)  VALUE
       NC"　　　　　　仕入伝票行数　＝　".
     03  MEI2-GYOSU               PIC       ZZZZZZ9.
*    検収明細行
 01  MEISAI3            CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(15)  VALUE
       NC"　　　　　　検収伝票枚数　＝　".
     03  MEI1-MAISU1              PIC       ZZZZZZ9.
*
 01  MEISAI4            CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(15)  VALUE
       NC"　　　　　　検収伝票行数　＝　".
     03  MEI2-GYOSU1              PIC       ZZZZZZ9.
*    支払伝票件数
 01  MEISAI5            CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(15)  VALUE
       NC"　　　　　　支払伝票件数　＝　".
     03  MEI1-MAISU2              PIC       ZZZZZZ9.
*    商品台帳件数
 01  MEISAI6            CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(15)  VALUE
       NC"　　　　　　商品台帳件数　＝　".
     03  MEI1-MAISU3              PIC       ZZZZZZ9.
*
******************************************************************
 PROCEDURE              DIVISION.
******************************************************************
 PROCESS                SECTION.
*
     PERFORM      INIT-SEC.
*
*****MOVE     ZERO      TO    END-FLG  NEW-DENNO  OLD-DENNO.
*****PERFORM      MAIN-SEC
*****             UNTIL    END-FLG = 1.
*
     MOVE     ZERO      TO    END-FLG  NEW-DENNO  OLD-DENNO.
     PERFORM      MAIN-SEC1
                  UNTIL    END-FLG = 1.
*
     MOVE     ZERO      TO    END-FLG.
     PERFORM      MAIN-SEC2
                  UNTIL    END-FLG = 1.
*
     MOVE     ZERO      TO    END-FLG.
     PERFORM      MAIN-SEC3
                  UNTIL    END-FLG = 1.
*
     PERFORM      END-SEC.
*
 PROCESS-END.
     EXIT.
******************************************************************
*                 INIT-SEC
******************************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     KENSYUD  SIHARAD  SHOHIND.
     OPEN     OUTPUT    PRINTF.
*
     ACCEPT   SYSYMD    FROM      DATE.
 INIT-END.
     EXIT.
******************************************************************
*                      MAIN-SEC
******************************************************************
*MAIN-SEC               SECTION.
*****READ     SHIRED
*****      AT END
*****         MOVE    1     TO    END-FLG
*****         GO      TO    MAIN-END
*****      NOT AT END
*****         MOVE    SSHI03      TO    NEW-DENNO
*****         ADD     1     TO    GYO-CNT
*****END-READ.
*
*****IF       NEW-DENNO      NOT =     OLD-DENNO
*****         ADD     1      TO        DEN-CNT
*****END-IF.
*
*****MOVE     NEW-DENNO      TO        OLD-DENNO.
*
*MAIN-END.
*****EXIT.
******************************************************************
*                      MAIN-SEC1
******************************************************************
 MAIN-SEC1              SECTION.
     READ     KENSYUD
           AT END
              MOVE    1     TO    END-FLG
              GO      TO    MAIN-END1
           NOT AT END
              MOVE    SKEN-REC01(4:9)  TO   NEW-DENNO
              ADD     1     TO    GYO-CNT1
     END-READ.
*
     IF       NEW-DENNO      NOT =     OLD-DENNO
              ADD     1      TO        DEN-CNT1
     END-IF.
*
     MOVE     NEW-DENNO      TO        OLD-DENNO.
*
 MAIN-END1.
     EXIT.
******************************************************************
*                      MAIN-SEC2
******************************************************************
 MAIN-SEC2              SECTION.
     READ     SIHARAD
           AT END
              MOVE    1     TO    END-FLG
              GO      TO    MAIN-END2
     END-READ.
*
     ADD      1              TO        DEN-CNT2.
*
 MAIN-END2.
     EXIT.
******************************************************************
*                      MAIN-SEC3
******************************************************************
 MAIN-SEC3              SECTION.
     READ     SHOHIND
           AT END
              MOVE    1     TO    END-FLG
              GO      TO    MAIN-END3
     END-READ.
*
     ADD      1              TO        DEN-CNT3.
*
 MAIN-END3.
     EXIT.
******************************************************************
*                      END-SEC
******************************************************************
 END-SEC                SECTION.
*
     MOVE     DEN-CNT        TO        MEI1-MAISU.
     MOVE     GYO-CNT        TO        MEI2-GYOSU.
     MOVE     DEN-CNT1       TO        MEI1-MAISU1.
     MOVE     GYO-CNT1       TO        MEI2-GYOSU1.
     MOVE     DEN-CNT2       TO        MEI1-MAISU2.
     MOVE     DEN-CNT3       TO        MEI1-MAISU3.
     MOVE     SYS-YY         TO        YY.
     MOVE     SYS-MM         TO        MM.
     MOVE     SYS-DD         TO        DD.
*
     WRITE    PRINT-REC      FROM      MIDASHI1 AFTER 2.
*****WRITE    PRINT-REC      FROM      MEISAI1  AFTER 4.
*****WRITE    PRINT-REC      FROM      MEISAI2  AFTER 2.
     WRITE    PRINT-REC      FROM      MEISAI3  AFTER 4.
     WRITE    PRINT-REC      FROM      MEISAI4  AFTER 2.
     WRITE    PRINT-REC      FROM      MEISAI5  AFTER 4.
     WRITE    PRINT-REC      FROM      MEISAI6  AFTER 4.
*
     CLOSE    KENSYUD  SIHARAD  SHOHIND.
     CLOSE    PRINTF.
*
     STOP     RUN.
********************<<  PROGRAM  END  >>**************************

```
