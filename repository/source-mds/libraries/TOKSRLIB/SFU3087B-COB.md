# SFU3087B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3087B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　                    *
*    モジュール名　　　　：　社内振替情報リスト出力ワーク作成　
*    　　　　　　　　　　　　（入力順出力）                    *
*    作成日／作成者　　　：　2018/05/14 T.TAKAHASHI            *
*    処理概要　　　　　　：　帳票・ＣＳＶ作成用に、振替情報・  *
*      　　　　　　　　　　　振替明細ファイルからレコード　　　*
*                            を抽出する。（入力順出力）        *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SFU3087B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/05/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*振替明細ファイル
     SELECT   SFRMEIL4  ASSIGN    TO        DA-01-VI-SFRMEIL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       MEI-F04
                                            MEI-F98
                                            MEI-F99
                        FILE  STATUS   IS   MEI-STATUS.
*振替情報ファイル
     SELECT   SFRHEDL1  ASSIGN    TO        DA-01-VI-SFRHEDL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HED-F01
                                            HED-F02
                                            HED-F03
                                            HED-F04
                                            HED-F05
                                            HED-F06
                                            HED-F07
                                            HED-F08
                        FILE  STATUS   IS   HED-STATUS.
*振替リスト出力ワーク
     SELECT   SFRLSTWK  ASSIGN    TO        DA-01-S-SFRLSTWK
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   LST-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    振替明細ファイル
******************************************************************
 FD  SFRMEIL4
                        LABEL RECORD   IS   STANDARD.
     COPY     SFRMEIL4  OF        XFDLIB
              JOINING   MEI  AS   PREFIX.
*
******************************************************************
*    振替情報ファイル
******************************************************************
 FD  SFRHEDL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SFRHEDL1  OF        XFDLIB
              JOINING   HED  AS   PREFIX.
*
******************************************************************
*    振替リストデータファイル
******************************************************************
 FD  SFRLSTWK           BLOCK     CONTAINS  15   RECORDS.
     COPY     SFRLSTWK  OF        XFDLIB
              JOINING   LST       PREFIX.
*****************************************************************
 WORKING-STORAGE        SECTION.
*ナフコ伝票データレイアウト
*    COPY   NFSHIRED OF XFDLIB  JOINING   NFD  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-MEI-F112             PIC  9(08)     VALUE  ZERO.
*
 01  WK-ST.
     03  MEI-STATUS        PIC  X(02).
     03  HED-STATUS        PIC  X(02).
     03  LST-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SFU3087B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFU3087B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFU3087B".
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
 01  PARA-IN-DTKBN          PIC   X(01).
 01  PARA-IN-INKBN          PIC   X(01).
 01  PARA-IN-KANKBN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-OUTSOKO
                                       PARA-IN-NYUF
                                       PARA-IN-NYUT
                                       PARA-IN-DTKBN
                                       PARA-IN-INKBN
                                       PARA-IN-KANKBN.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRMEIL4.
     MOVE      "SFRMEIL4"   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRHEDL1.
     MOVE      "SFRHEDL1"   TO   AB-FILE.
     MOVE      HED-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
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
     OPEN     INPUT     SFRMEIL4  SFRHEDL1.
     OPEN     OUTPUT    SFRLSTWK.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
     MOVE     SPACE           TO   MEI-REC.
     INITIALIZE                    MEI-REC.
     MOVE     PARA-IN-OUTSOKO TO   MEI-F04.
     MOVE     PARA-IN-NYUF    TO   MEI-F98.
     START    SFRMEIL4  KEY   >=   MEI-F04   MEI-F98   MEI-F99
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     SFRMEIL4
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
*    倉庫・入力日チェック
     IF     ( PARA-IN-OUTSOKO  =    MEI-F04 ) AND
            ( PARA-IN-NYUF    <=    MEI-F98 ) AND
            ( PARA-IN-NYUT    >=    MEI-F98 )
              CONTINUE
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*
*    ＤＴ区分チェック
     IF       PARA-IN-DTKBN    =    SPACE
              CONTINUE
     ELSE
              IF   PARA-IN-DTKBN    =    "1"
                   IF   MEI-F11          =    "1"
                        CONTINUE
                   ELSE
                        GO        TO        MAIN-010
                   END-IF
              END-IF
              IF   PARA-IN-DTKBN    =    "2"
                   IF   MEI-F11          =    "2"
                        CONTINUE
                   ELSE
                        GO        TO        MAIN-010
                   END-IF
              END-IF
     END-IF.
*
*    入力区分チェック
     IF       PARA-IN-INKBN    =    SPACE
              CONTINUE
     ELSE
              IF   PARA-IN-INKBN    =    "1"
                   IF   MEI-F91          =    " "
                        CONTINUE
                   ELSE
                        GO        TO        MAIN-010
                   END-IF
              END-IF
              IF   PARA-IN-INKBN    =    "2"
                   IF   MEI-F91          =    "1"
                        CONTINUE
                   ELSE
                        GO        TO        MAIN-010
                   END-IF
              END-IF
     END-IF.
*
 MAIN-002.
*    振分情報ファイル検索
     MOVE     MEI-F01        TO   HED-F01.
     MOVE     MEI-F02        TO   HED-F02.
     MOVE     MEI-F03        TO   HED-F03.
     MOVE     MEI-F04        TO   HED-F04.
     MOVE     MEI-F05        TO   HED-F05.
     MOVE     MEI-F06        TO   HED-F06.
     MOVE     MEI-F07        TO   HED-F07.
     MOVE     MEI-F08        TO   HED-F08.
     READ     SFRHEDL1
          INVALID
              DISPLAY NC"振分情報ファイルなし！" UPON CONS
              DISPLAY NC"伝票区分　＝" MEI-F01   UPON CONS
              DISPLAY NC"年度　　　＝" MEI-F02   UPON CONS
              DISPLAY NC"シーズン　＝" MEI-F03   UPON CONS
              DISPLAY NC"倉庫ＣＤ　＝" MEI-F04   UPON CONS
              DISPLAY NC"商品ＣＤ　＝" MEI-F05   UPON CONS
              DISPLAY NC"品単１　　＝" MEI-F06   UPON CONS
              DISPLAY NC"品単２　　＝" MEI-F07   UPON CONS
              DISPLAY NC"品単３　　＝" MEI-F08   UPON CONS
              MOVE    4010    TO   PROGRAM-STATUS
              STOP    RUN
     END-READ.
*
 MAIN-003.
*
*    完納区分チェック
     IF       PARA-IN-KANKBN   =    SPACE
              CONTINUE
     ELSE
              IF   PARA-IN-KANKBN   =    "1"
                   IF   HED-F24          =    " "
                        CONTINUE
                   ELSE
                        GO        TO        MAIN-010
                   END-IF
              END-IF
              IF   PARA-IN-KANKBN   =    "2"
                   IF   HED-F24          =    "1"
                        CONTINUE
                   ELSE
                        GO        TO        MAIN-010
                   END-IF
              END-IF
     END-IF.
*
 MAIN-004.
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
     MOVE     MEI-F11        TO   LST-MF11.
*   発注日／入荷日
     MOVE     MEI-F12        TO   LST-MF12.
*   棚番
     MOVE     MEI-F13        TO   LST-MF13.
*   発注／入荷数
     MOVE     MEI-F14        TO   LST-MF14.
*   ＭＳＧ
     MOVE     MEI-F15        TO   LST-MF15.
*   予備領域
*   入力区分
     MOVE     MEI-F91        TO   LST-MF91.
*   登録者部門
     MOVE     MEI-F92        TO   LST-MF92.
*   登録者担当者CD
     MOVE     MEI-F93        TO   LST-MF93.
*   登録日付
     MOVE     MEI-F94        TO   LST-MF94.
*   登録時刻
     MOVE     MEI-F95        TO   LST-MF95.
*   更新者部門
     MOVE     MEI-F96        TO   LST-MF96.
*   更新者担当者CD
     MOVE     MEI-F97        TO   LST-MF97.
*   更新日付
     MOVE     MEI-F98        TO   LST-MF98.
*   更新時刻
     MOVE     MEI-F99        TO   LST-MF99.
*
     WRITE    LST-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     READ     SFRMEIL4
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
     CLOSE     SFRMEIL4  SFRHEDL1  SFRLSTWK.
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
