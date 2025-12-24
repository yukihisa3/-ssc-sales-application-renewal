# SKY1404B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1404B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　オフライン出荷                    *
*    業務名　　　　　　　：　スポット出荷処理                  *
*    モジュール名　　　　：　スポット出荷ＤＴ→ピッキング作成  *
*    作成日／更新日　　　：　1999/10/19                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　スポット出荷ファイルより、ピッキン*
*                            グデータを作成する。              *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKY1404B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/10/19.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*スポット出荷ファイル
     SELECT   JHTSPTF   ASSIGN    TO        DA-01-VI-JHTSPTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SPT-F011
                                            SPT-F012
                                            SPT-F02
                        FILE  STATUS   IS   SPT-STATUS.
*スポットピッキングデータ
     SELECT   SPOTWK    ASSIGN    TO        DA-01-S-SPOTWK
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SPW-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    スポット出荷ファイル
******************************************************************
 FD  JHTSPTF
                        LABEL RECORD   IS   STANDARD.
     COPY     JHTSPTF   OF        XFDLIB
              JOINING   SPT  AS   PREFIX.
*
******************************************************************
*    スポットピッキングデータ
******************************************************************
 FD  SPOTWK             BLOCK     CONTAINS  5    RECORDS.
     COPY     SPOTWK    OF        XFDLIB
              JOINING   SPW       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
*
 01  WK-ST.
     03  SPT-STATUS        PIC  X(02).
     03  SPW-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKY1404B".
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
******************************************************************
 LINKAGE                SECTION.
 01  LINK-SOKCD             PIC   9(02).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION    USING    LINK-SOKCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHTSPTF.
     MOVE      "JHTSPTF"    TO   AB-FILE.
     MOVE      SPT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SPOTWK.
     MOVE      "SPOTEK  "   TO   AB-FILE.
     MOVE      SPW-STATUS   TO   AB-STS.
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
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     JHTSPTF   TOKMS2.
     OPEN     OUTPUT    SPOTWK.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.

     READ     JHTSPTF
              AT END     MOVE     9         TO  END-FG
              NOT AT END ADD      1         TO  RD-CNT
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
*出荷明細ワーク出力
     MOVE     SPACE          TO   SPW-REC.
     INITIALIZE                   SPW-REC.
*メモ番号１
     MOVE     SPT-F011       TO   SPW-F201.
*メモ番号２
     MOVE     SPT-F012       TO   SPW-F202.
*取引先ｺｰﾄﾞ
     MOVE     SPT-F11        TO   SPW-F013.
*倉庫ｺｰﾄﾞ
     MOVE     LINK-SOKCD     TO   SPW-F03.
*発注日
     MOVE     SPT-F071       TO   SPW-F05.
*納品日
     MOVE     SPT-F072       TO   SPW-F06.
*商品コード
     MOVE     SPT-F62(1:8)   TO   SPW-F081.
*品単コード
     MOVE     SPT-F62(9:8)   TO   SPW-F082.
*_番
     MOVE     SPT-F06        TO   SPW-F09.
*商品名称
     MOVE     SPT-F1211      TO   SPW-F111.
     MOVE     SPT-F1212      TO   SPW-F112.
*原価単価
     MOVE     SPT-F131       TO   SPW-F15
*売価単価
     MOVE     SPT-F132       TO   SPW-F16
*得意先マスタ検索
     MOVE     SPT-F11        TO   TOK-F01
     READ     TOKMS2
         INVALID
              MOVE ALL "*"   TO   SPW-F17
         NOT  INVALID
              MOVE TOK-F04   TO   SPW-F17
     END-READ.
*テーブルによりレコード作成
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 50
             IF   SPT-F162(IX)  NOT =  ZERO
                  MOVE    SPT-F161(IX)  TO   SPW-F02
                  MOVE    SPT-F162(IX)  TO   SPW-F13
                  WRITE   SPW-REC
                  ADD     1             TO   WRT-CNT
             END-IF
     END-PERFORM.
*
 MAIN-010.
     READ     JHTSPTF
              AT END     MOVE     9         TO  END-FG
              NOT AT END ADD      1         TO  RD-CNT
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
*
     CLOSE     JHTSPTF   TOKMS2   SPOTWK.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
