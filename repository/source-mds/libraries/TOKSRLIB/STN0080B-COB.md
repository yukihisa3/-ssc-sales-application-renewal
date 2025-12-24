# STN0080B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STN0080B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　_卸ローカル運用　　　　　　　　  *
*    モジュール名　　　　：　倉庫_卸結果データ取込　　　　　　*
*    作成日／作成者　　　：　2021/03/15 INOUE                  *
*    処理概要　　　　　　：　倉庫から転送された_卸データを　　*
*                            基幹に取り込む　　　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STN0080B.
*                  流用:STN0040B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/03/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 基幹連携データ >>--*
     SELECT   SKTANAXX  ASSIGN    TO        DA-01-VS-SKTANAXX
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   SKT-STATUS.
*---<< SUB商品名称マスタ >>
     SELECT   SUBMEIL7  ASSIGN  TO   DA-01-VI-SUBMEIL7
                        ORGANIZATION         IS   INDEXED
                        ACCESS  MODE         IS   RANDOM
                        RECORD  KEY          IS   MEI-D01
                        FILE    STATUS       IS   MEI-STATUS.
*----<< _卸結果データ >>----*
     SELECT   SKTANAL1  ASSIGN    TO        DA-01-VI-SKTANAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       OUT-F01
                                            OUT-F02
                                            OUT-F05
                                            OUT-F06
                                            OUT-F07
                        FILE      STATUS    OUT-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基幹連携データ
******************************************************************
 FD  SKTANAXX            LABEL     RECORD   IS   STANDARD.
     COPY     SKTANAXX   OF        XFDLIB
              JOINING   SKT       PREFIX.
******************************************************************
*    SUB商品名称マスタ
******************************************************************
 FD    SUBMEIL7
       LABEL     RECORD    IS    STANDARD.
       COPY      SUBMEIL7    OF    XFDLIB
                 JOINING   MEI   PREFIX.
******************************************************************
*    _卸結果データ
******************************************************************
 FD  SKTANAL1           LABEL     RECORD   IS   STANDARD.
     COPY     SKTANAL1  OF        XFDLIB
              JOINING  OUT  AS   PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  SKTANAL1-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SUBMEIL7-INV-FLG    PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*日付の編集
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
 01  WK-ST.
     03  SKT-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  OUT-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STN0080B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STN0080B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STN0080B".
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
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
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
 01  PARA-IN-TANCD             PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING
                                  PARA-IN-TANCD.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SKTANAXX.
     MOVE      "SKTANAXX"    TO   AB-FILE.
     MOVE      SKT-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBMEIL7.
     MOVE      "SUBMEIL7"   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SKTANAL1.
     MOVE      "SKTANAL1"   TO   AB-FILE.
     MOVE      OUT-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SKTANAXX  SUBMEIL7.
     OPEN     I-O       SKTANAL1.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
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
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    基幹連携データ読込み
     PERFORM SKTANAXX-READ-SEC.
     IF      END-FLG  NOT = "END"
             CONTINUE
     ELSE
             DISPLAY NC"対象データありません" UPON CONS
             MOVE     4010          TO  PROGRAM-STATUS
             STOP     RUN
     END-IF.
*
*    _卸結果データ　スタート
     MOVE      SPACE           TO   OUT-REC.
     INITIALIZE                     OUT-REC.
     MOVE      SKT-F01         TO   OUT-F01.
     MOVE      SKT-F02         TO   OUT-F02.
     MOVE      SKT-F05         TO   OUT-F05.
     MOVE      SKT-F06         TO   OUT-F06.
     MOVE      SKT-F07         TO   OUT-F07.
     START     SKTANAL1  KEY   >=   OUT-F01  OUT-F02
                                    OUT-F05  OUT-F06  OUT-F07
         INVALID   KEY
*             MOVE    "END"  TO   END-FLG
              GO             TO   INIT-99
     END-START.
*    _卸結果データ　ＲＥＡＤ
     READ     SKTANAL1
         NEXT AT END
              GO             TO   INIT-99
     END-READ.
     IF     ( SKT-F01   =    OUT-F01 ) AND
            ( SKT-F02   =    OUT-F02 )
              DISPLAY NC"既に取込済の倉庫です" UPON CONS
              DISPLAY NC"　_卸日＝" SKT-F01   UPON CONS
              DISPLAY NC"　倉庫　＝" SKT-F02   UPON CONS
              MOVE    4000   TO      PROGRAM-STATUS
              STOP    RUN
     END-IF.
*
 INIT-99.
     CLOSE    SKTANAL1.
     OPEN I-O SKTANAL1.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SKTANAXX-READ-SEC    SECTION.
*
     MOVE    "SKTANAXX-READ-SEC"    TO  S-NAME.
*
     READ     SKTANAXX
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  SKTANAXX-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 SKTANAXX-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
     MOVE    SKT-F01                 TO  OUT-F01.
     MOVE    SKT-F02                 TO  OUT-F02.
     MOVE    SKT-F03                 TO  OUT-F03.
     MOVE    SKT-F04                 TO  OUT-F04.
     MOVE    SKT-F05                 TO  OUT-F05.
     MOVE    SKT-F06                 TO  OUT-F06.
     MOVE    SKT-F07                 TO  OUT-F07
                                         MEI-D01.
     READ    SUBMEIL7
        INVALID
             MOVE    SPACE           TO  OUT-F08
                                         OUT-F09
        NOT  INVALID
             MOVE    MEI-F011        TO  OUT-F08
             MOVE    MEI-F012        TO  OUT-F09
     END-READ.
     MOVE    SKT-F08                 TO  OUT-F10.
     MOVE    SKT-F09                 TO  OUT-F11.
     MOVE    SKT-F10                 TO  OUT-F12.
     MOVE    SYS-DATE                TO  OUT-F97.
     MOVE    WK-TIME                 TO  OUT-F98.
     MOVE    PARA-IN-TANCD           TO  OUT-F99.
     WRITE   OUT-REC.
     ADD     1                       TO  WRT-CNT.
*
 MAIN999.
*    基幹連携データ読込み
     PERFORM SKTANAXX-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY   NC"基幹連携データ" "IN  = " READ-CNT  UPON CONS.
     DISPLAY   NC"_卸結果データ" "OUT = " WRT-CNT   UPON CONS.
*    DISPLAY   NC"抽出対象外" "SKIP= " SKIP-CNT  UPON CONS.
*
     IF        WRT-CNT  =  ZERO
               MOVE    4010    TO    PROGRAM-STATUS
               STOP    RUN
     END-IF.
*
     CLOSE     SKTANAXX  SUBMEIL7  SKTANAL1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
