# SSY0006A

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY0006A.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　発注集計表                        *
*    モジュール名　　　　：　発注集計表データ抽出（商品順）    *
*    作成日／更新日　　　：　99/09/14                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、該当    *
*                            のデータを売上伝票データファイル  *
*                            より抽出する。                    *
*　　更新日／更新者　　　：　99/12/27  /  HAGIWARA             *
*　　更新日／更新者　　　：　11/10/06  /  YOSHIDA.M            *
*    更新概要　　　　　　：　基幹サーバ統合                    *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY0006A.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
***2011.10.06(DEN-F07,DEN-F112)
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*出荷明細ワークファイル
     SELECT   SHWHACF   ASSIGN    TO        DA-01-S-SHWHACF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HAC-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    発注集計表データファイル
******************************************************************
 FD  SHWHACF            BLOCK     CONTAINS  5    RECORDS.
     COPY     SHWHACF   OF        XFDLIB
              JOINING   HAC       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*****************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HAC-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY0006A".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0006A".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0006A".
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
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-TKBN              PIC   9(01).
 01  PARA-OUTNO             PIC   9(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TOKCD
                                       PARA-SOKO
                                       PARA-TKBN
                                       PARA-OUTNO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHWHACF.
     MOVE      "SHWHACF "   TO   AB-FILE.
     MOVE      HAC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENLA  TOKMS2  SHOTBL1.
     OPEN     OUTPUT    SHWHACF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*    得意先マスタ検索
     MOVE     SPACE          TO   TOK-REC
     INITIALIZE                   TOK-REC
     MOVE     PARA-TOKCD     TO   TOK-F01
     READ     TOKMS2
         INVALID
              MOVE SPACE     TO   TOK-REC
              INITIALIZE          TOK-REC
     END-READ.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TOKCD     TO   DEN-F01.
     MOVE     PARA-SOKO      TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
***2011.10.06(DEN-F07,DEN-F112)
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     SHTDENLA
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1    TO   RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     IF     ( PARA-JDATE     =    DEN-F46 ) AND
            ( PARA-JTIME     =    DEN-F47 ) AND
            ( PARA-TOKCD     =    DEN-F01 )
              CONTINUE
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*    倉庫コード
*****IF       PARA-SOKO      NOT =     ZERO
     IF       PARA-SOKO      NOT =     SPACE
              IF   DEN-F48   =    PARA-SOKO
                   CONTINUE
              ELSE
                   GO   TO        MAIN-010
              END-IF
     ELSE
              CONTINUE
     END-IF.
*
*出荷明細ワーク出力
     MOVE     SPACE          TO   HAC-REC.
     INITIALIZE                   HAC-REC.
     MOVE     DEN-F46        TO   HAC-F011.
     MOVE     DEN-F47        TO   HAC-F012.
     MOVE     DEN-F01        TO   HAC-F013.
     MOVE     DEN-F07        TO   HAC-F02.
*---------  1999/12/27 修正 START ------------*
*****MOVE     DEN-F48        TO   HAC-F03.
     MOVE     DEN-F42        TO   HAC-F20.
*****トステムビバ特別処理
     IF       DEN-F01  =  38709  OR  387091
*             IF  DEN-F25  =  30849590
*             OR  DEN-F25  =  30849604
*             OR  DEN-F25  =  30391764
*             OR  DEN-F25  =  30391772
*             OR  DEN-F25  =  30775023
*             OR  DEN-F25  =  30807863
              IF  DEN-F25  =  36426420
              OR  DEN-F25  =  36426438
              OR  DEN-F25  =  36426446
*             OR  DEN-F25  =  91831406
*             OR  DEN-F25  =  91831503
              OR  DEN-F25  =  92578607
*             OR  DEN-F25  =  96771207
*             OR  DEN-F25  =  90908804
*             OR  DEN-F25  =  90908812
              OR  DEN-F25  =  97597707
              OR  DEN-F25  =  97597804
              OR  DEN-F25  =  97597600
*             OR  DEN-F25  =  36473355
              OR  DEN-F25  =  36458356
                  MOVE  "ﾃﾝﾎﾟﾁｮｸｿｳ"   TO   HAC-F20
              END-IF
     END-IF.
*---------  1999/12/27 修正 END   ------------*
     MOVE     DEN-F12        TO   HAC-F04.
     MOVE     DEN-F111       TO   HAC-F05.
     MOVE     DEN-F112       TO   HAC-F06.
     MOVE     DEN-F25        TO   HAC-F07.
     MOVE     DEN-F1411      TO   HAC-F081.
     MOVE     DEN-F1412      TO   HAC-F082.
*---------  2000/03/14 修正 START ------------*
*  商品変換テーブル検索
     MOVE     DEN-F01        TO    TBL-F01.
     MOVE     DEN-F25        TO    TBL-F02.
     READ    SHOTBL1
       INVALID
              MOVE  SPACE    TO    HAC-F09
       NOT INVALID
              MOVE  TBL-F08  TO    HAC-F09
              MOVE  TBL-F031 TO    HAC-F081
              MOVE  TBL-F032 TO    HAC-F082
     END-READ.
*****MOVE     DEN-F49        TO   HAC-F09.
*---------  2000/03/14 修正 END   ------------*
     MOVE     SPACE          TO   HAC-F10.
     MOVE     DEN-F1421      TO   HAC-F111.
     MOVE     DEN-F1422      TO   HAC-F112.
     MOVE     SPACE          TO   HAC-F12.
     MOVE     DEN-F50        TO   HAC-F13.
     MOVE     DEN-F15        TO   HAC-F14.
     IF       PARA-TKBN      =    1
              MOVE      DEN-F512       TO   HAC-F15
              MOVE      DEN-F513       TO   HAC-F16
              MOVE      DEN-F521       TO   HAC-F18
              MOVE      DEN-F522       TO   HAC-F19
     ELSE
              MOVE      DEN-F172       TO   HAC-F15
              MOVE      DEN-F173       TO   HAC-F16
              MOVE      DEN-F181       TO   HAC-F18
              MOVE      DEN-F182       TO   HAC-F19
     END-IF.
     MOVE     TOK-F04        TO   HAC-F17.
     WRITE    HAC-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     READ     SHTDENLA
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                   ADD  1    TO   RD-CNT
     END-READ.
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
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SHTDENLA  TOKMS2  SHWHACF  SHOTBL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
