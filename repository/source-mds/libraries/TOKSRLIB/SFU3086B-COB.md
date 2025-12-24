# SFU3086B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3086B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　                    *
*    モジュール名　　　　：　社内振替情報リスト出力ワーク作成２
*    　　　　　　　　　　　　（データ抽出）                    *
*    作成日／作成者　　　：　2016/01/15 INOUE                  *
*    処理概要　　　　　　：　帳票・ＣＳＶ作成用に、            *
*      　　　　　　　　　　　振替情報ファイルからレコード　　　*
*                            を抽出する。　                    *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SFU3086B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/01/15.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*振替情報ファイル
     SELECT   SFRHEDL2  ASSIGN    TO        DA-01-VI-SFRHEDL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HED-F04
                                            HED-F14
                                            HED-F02
                                            HED-F03
                        FILE  STATUS   IS   HED-STATUS.
*振替リスト出力ワーク
     SELECT   SFRLSTWK  ASSIGN    TO        DA-01-S-SFRLSTWK
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   LST-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    振替情報ファイル
******************************************************************
 FD  SFRHEDL2
                        LABEL RECORD   IS   STANDARD.
     COPY     SFRHEDL2  OF        XFDLIB
              JOINING   HED  AS   PREFIX.
*
******************************************************************
*    振替リストデータファイル
******************************************************************
 FD  SFRLSTWK
                        LABEL RECORD   IS   STANDARD.
     COPY     SFRLSTWK  OF        XFDLIB
              JOINING   LST       PREFIX.
*****************************************************************
 WORKING-STORAGE        SECTION.
*
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-HED-F112             PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  HED-STATUS        PIC  X(02).
     03  LST-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SFU3086B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFU3086B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFU3086B".
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
*------------------------------------------------------------
 LINKAGE                SECTION.
 01  PARA-IN-OUTSOKO        PIC   X(02).
 01  PARA-IN-NYUF           PIC   9(08).
 01  PARA-IN-NYUT           PIC   9(08).
 01  PARA-IN-NENDOF         PIC   9(04).
 01  PARA-IN-NENDOT         PIC   9(04).
 01  PARA-IN-SEASONF        PIC   X(02).
 01  PARA-IN-SEASONT        PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-OUTSOKO
                                       PARA-IN-NYUF
                                       PARA-IN-NYUT
                                       PARA-IN-NENDOF
                                       PARA-IN-NENDOT
                                       PARA-IN-SEASONF
                                       PARA-IN-SEASONT.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRHEDL2.
     MOVE      "SFRHEDL2"   TO   AB-FILE.
     MOVE      HED-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRLSTWK.
     MOVE      "SFRLSTWK "  TO   AB-FILE.
     MOVE      LST-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SFRHEDL2.
     OPEN     OUTPUT    SFRLSTWK.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE           TO   HED-REC.
     INITIALIZE                    HED-REC.
*
     MOVE     PARA-IN-OUTSOKO TO   HED-F04.
     MOVE     PARA-IN-NYUF    TO   HED-F14.
     MOVE     PARA-IN-NENDOF  TO   HED-F02.
     MOVE     PARA-IN-SEASONF TO   HED-F03.
*
     START    SFRHEDL2  KEY   >=   HED-F04
                                   HED-F14
                                   HED-F02
                                   HED-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     SFRHEDL2
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
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
 MAIN-001.
*    倉庫・入荷予定日・年度・シーズン　チェック
     IF     ( PARA-IN-OUTSOKO  =    HED-F04 ) AND
            ( PARA-IN-NYUF    <=    HED-F14 ) AND
            ( PARA-IN-NYUT    >=    HED-F14 ) AND
            ( PARA-IN-NENDOF  <=    HED-F02 ) AND
            ( PARA-IN-NENDOT  >=    HED-F02 ) AND
            ( PARA-IN-SEASONF <=    HED-F03 ) AND
            ( PARA-IN-SEASONT >=    HED-F03 ) AND
            ( " "              =    HED-F24 )
              CONTINUE
     ELSE
              IF     ( PARA-IN-OUTSOKO NOT =    HED-F04 ) OR
                     ( PARA-IN-NYUT       <=    HED-F14 )
*                    ( PARA-IN-NENDOT     <=    HED-F02 )
                       MOVE      9         TO   END-FG
                       GO                  TO   MAIN-EXIT
              ELSE
                       GO                  TO   MAIN-010
              END-IF
     END-IF.
*
 MAIN-002.
*振替リスト出力ワーク出力
     MOVE     SPACE          TO   LST-REC.
     INITIALIZE                   LST-REC.
*共通部
*   伝票区分
     MOVE     HED-F01        TO   LST-F01.
*   年度
     MOVE     HED-F02        TO   LST-F02.
*   シーズン
     MOVE     HED-F03        TO   LST-F03.
*   倉庫CD
     MOVE     HED-F04        TO   LST-F04.
*   サカタ商品CD
     MOVE     HED-F05        TO   LST-F05.
*   サカタ品単１
     MOVE     HED-F06        TO   LST-F06.
*   サカタ品単２
     MOVE     HED-F07        TO   LST-F07.
*   サカタ品単３
     MOVE     HED-F08        TO   LST-F08.
*ヘッダ部
*   商品名１
     MOVE     HED-F09        TO   LST-F09.
*   商品名２
     MOVE     HED-F10        TO   LST-F10.
*   棚番
     MOVE     HED-F11        TO   LST-F11.
*   仕入先CD
     MOVE     HED-F12        TO   LST-F12.
*   ＪＡＮＣＤ
     MOVE     HED-F13        TO   LST-F13.
*   入荷予定日
     MOVE     HED-F14        TO   LST-F14.
*   発注数合計
     MOVE     HED-F15        TO   LST-F15.
*   最終発注担当者部門
     MOVE     HED-F16        TO   LST-F16.
*   最終発注担当者CD
     MOVE     HED-F17        TO   LST-F17.
*   最終発注日
     MOVE     HED-F18        TO   LST-F18.
*   入荷数合計
     MOVE     HED-F19        TO   LST-F19.
*   最終入荷担当者部門
     MOVE     HED-F20        TO   LST-F20.
*   最終入荷担当者CD
     MOVE     HED-F21        TO   LST-F21.
*   最終入荷日
     MOVE     HED-F22        TO   LST-F22.
*   ＭＳＧ
     MOVE     HED-F23        TO   LST-F23.
*   完納区分
     MOVE     HED-F24        TO   LST-F24.
*   完納日
     MOVE     HED-F25        TO   LST-F25.
*   完納者部門
     MOVE     HED-F26        TO   LST-F26.
*   完納担当者CD
     MOVE     HED-F27        TO   LST-F27.
*   予備領域
*   入力区分
     MOVE     HED-F91        TO   LST-F91.
*   登録者部門
     MOVE     HED-F92        TO   LST-F92.
*   登録者担当者CD
     MOVE     HED-F93        TO   LST-F93.
*   登録日付
     MOVE     HED-F94        TO   LST-F94.
*   登録時刻
     MOVE     HED-F95        TO   LST-F95.
*   更新者部門
     MOVE     HED-F96        TO   LST-F96.
*   更新者担当者CD
     MOVE     HED-F97        TO   LST-F97.
*   更新日付
     MOVE     HED-F98        TO   LST-F98.
*   更新時刻
     MOVE     HED-F99        TO   LST-F99.
*明細部
*   発注入荷区分
*   発注日／入荷日
*   棚番
*   発注／入荷数
*   ＭＳＧ
*   予備領域
*   入力区分
*   登録者部門
*   登録者担当者CD
*   登録日付
*   登録時刻
*   更新者部門
*   更新者担当者CD
*   更新日付
*   更新時刻
*
     WRITE    LST-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     READ     SFRHEDL2
              AT  END    MOVE      9         TO  END-FG
              NOT AT  END
                  ADD 1    TO   RD-CNT
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
     CLOSE     SFRHEDL2  SFRLSTWK.
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
