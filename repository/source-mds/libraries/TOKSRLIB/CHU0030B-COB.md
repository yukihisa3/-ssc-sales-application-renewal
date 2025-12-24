# CHU0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/CHU0030B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　注文日別データ抽出（手書き　　）　*
*    作成日／更新日　　　：　23/07/20                          *
*    作成者／更新者　　　：　ＮＡＶ三浦　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取ったパラメタの抽出日        *
*                            より該当のデータを抽出する。      *
*                                                              *
*    更新日／更新者　　　：　2024/07/30 NAV TAKAHASHI          *
*    修正概要　　　　　　：　抽出条件Ｆ４８→Ｆ０８へ変更      *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            CHU0030B.
 AUTHOR.                NAV MIURA.
 DATE-WRITTEN.          23/07/20.
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
     SELECT   SHTDENLU  ASSIGN    TO        DA-01-VI-SHTDENLU
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F274  DEN-F62
                                            DEN-F01   DEN-F08
                        FILE      STATUS    IS   DEN-STATUS.
*注文日別受注抽出データ
     SELECT   CHUMONL1  ASSIGN    TO        DA-01-VI-CHUMONL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       MON-F01   MON-F02
                                            MON-F04   MON-F051
                                            MON-F07   MON-F112
                                            MON-F03
                        FILE      STATUS    IS   MON-STATUS.
*SUB商品名称マスタ
     SELECT   SUBMEIL1  ASSIGN    TO        DA-01-VI-SUBMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011  MEI-F0121
                                            MEI-F0122 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLU
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    注文日別受注抽出データ
******************************************************************
 FD  CHUMONL1           LABEL RECORD   IS   STANDARD.
     COPY     CHUMONF   OF        XFDLIB
              JOINING   MON       PREFIX.
******************************************************************
*    SUB商品名称マスタ
******************************************************************
 FD  SUBMEIL1           LABEL RECORD   IS   STANDARD.
     COPY     SUBMEIF   OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-CATEGORY             PIC  X(02)     VALUE  SPACE.
 01  WK-BUNRUI               PIC  X(01)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  MON-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "CHU0030B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "CHU0030B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "CHU0030B".
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
 01  PARA-STORICD           PIC   9(08).
 01  PARA-ETORICD           PIC   9(08).
 01  PARA-SCDATE            PIC   9(08).
 01  PARA-ECDATE            PIC   9(08).
 01  PARA-SSOKO             PIC   X(02).
 01  PARA-ESOKO             PIC   X(02).
 01  PARA-CATEGORY1         PIC   X(02).
 01  PARA-CATEGORY2         PIC   X(02).
 01  PARA-CATEGORY3         PIC   X(02).
 01  PARA-BUNRUI1           PIC   X(01).
 01  PARA-BUNRUI2           PIC   X(01).
 01  PARA-BUNRUI3           PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-STORICD
                                       PARA-ETORICD
                                       PARA-SCDATE
                                       PARA-ECDATE
                                       PARA-SSOKO
                                       PARA-ESOKO
                                       PARA-CATEGORY1
                                       PARA-CATEGORY2
                                       PARA-CATEGORY3
                                       PARA-BUNRUI1
                                       PARA-BUNRUI2
                                       PARA-BUNRUI3.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLU.
     MOVE      "SHTDENLU"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CHUMONL1.
     MOVE      "CHUMONL1 "   TO   AB-FILE.
     MOVE      MON-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     DISPLAY   "MON-F01  = " MON-F01  UPON CONS.
     DISPLAY   "MON-F02  = " MON-F02  UPON CONS.
     DISPLAY   "MON-F04  = " MON-F04  UPON CONS.
     DISPLAY   "MON-F051 = " MON-F051 UPON CONS.
     DISPLAY   "MON-F07  = " MON-F07  UPON CONS.
     DISPLAY   "MON-F112 = " MON-F112 UPON CONS.
     DISPLAY   "MON-F03  = " MON-F03  UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SUBMEIL1.
     MOVE      "SUBMEIL1 "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SHTDENLU  SUBMEIL1.
     OPEN     EXTEND    CHUMONL1.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
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
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     ZERO           TO   DEN-F274.
     MOVE     PARA-SCDATE    TO   DEN-F62.
     START    SHTDENLU  KEY  >=   DEN-F274  DEN-F62
                                  DEN-F01   DEN-F08
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     READ     SHTDENLU
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
*オンライン＝０以外
     IF     DEN-F274   = ZERO
            CONTINUE
     ELSE
         MOVE      9         TO   END-FG
         GO        TO        MAIN-EXIT
     END-IF.
*抽出日対象外で終了
     IF     ( DEN-F62       >=  PARA-SCDATE ) AND
            ( DEN-F62       <=  PARA-ECDATE )
            CONTINUE
     ELSE
         MOVE      9         TO   END-FG
         GO        TO        MAIN-EXIT
     END-IF.
*行番号＜80
     IF     DEN-F03 < 80
            CONTINUE
     ELSE
            GO   TO   MAIN-010
     END-IF.
*伝区＝40
     IF     DEN-F051 = 40
            CONTINUE
     ELSE
            GO   TO   MAIN-010
     END-IF.
*取引先コード対象外は読み飛ばし
     IF ( DEN-F01       >=  PARA-STORICD ) AND
        ( DEN-F01       <=  PARA-ETORICD )
               CONTINUE
     ELSE
               GO   TO   MAIN-010
     END-IF.
*倉庫コード対象外は読み飛ばし
*#2024/07/30 NAV ST　オンラインの出荷場所から出場に変更
*****IF ( DEN-F48       >=   PARA-SSOKO ) AND
*****   ( DEN-F48       <=   PARA-ESOKO )
     IF ( DEN-F08       >=   PARA-SSOKO ) AND
        ( DEN-F08       <=   PARA-ESOKO )
*#2024/07/30 NAV ED
               CONTINUE
     ELSE
               GO   TO   MAIN-010
     END-IF.
*商品コード空白は対象外
     IF     DEN-F1411   NOT =  SPACE
            CONTINUE
     ELSE
            GO   TO   MAIN-010
     END-IF.
*
*  SUB商品名称マスタ検索
     MOVE     DEN-F1411      TO   MEI-F011.
     MOVE     DEN-F1412      TO   MEI-F012.
     READ     SUBMEIL1
       INVALID
              GO   TO   MAIN-010
       NOT  INVALID
              MOVE MEI-F09   TO   WK-CATEGORY
              MOVE MEI-F90   TO   WK-BUNRUI
     END-READ.
*
*商品カテゴリチェック
     IF (  PARA-CATEGORY1 = SPACE ) AND
        (  PARA-CATEGORY2 = SPACE ) AND
        (  PARA-CATEGORY3 = SPACE )
           CONTINUE
     ELSE
         IF (  PARA-CATEGORY1 = WK-CATEGORY ) OR
            (  PARA-CATEGORY2 = WK-CATEGORY ) OR
            (  PARA-CATEGORY3 = WK-CATEGORY )
               CONTINUE
         ELSE
               GO   TO   MAIN-010
         END-IF
     END-IF.
*分類区分チェック
     IF (  PARA-BUNRUI1 = SPACE ) AND
        (  PARA-BUNRUI2 = SPACE ) AND
        (  PARA-BUNRUI3 = SPACE )
           CONTINUE
     ELSE
         IF (  PARA-BUNRUI1 = WK-BUNRUI ) OR
            (  PARA-BUNRUI2 = WK-BUNRUI ) OR
            (  PARA-BUNRUI3 = WK-BUNRUI )
               CONTINUE
         ELSE
               GO   TO   MAIN-010
         END-IF
     END-IF.

*注文日別受注抽出データ
     MOVE     SPACE          TO   MON-REC.
     INITIALIZE                   MON-REC.
     MOVE     DEN-REC        TO   MON-REC.
     MOVE     WK-CATEGORY    TO   MON-F70.
     MOVE     WK-BUNRUI      TO   MON-F71.
     WRITE    MON-REC.
     ADD      1              TO   WRT-CNT.
*
 MAIN-010.
*
     READ     SHTDENLU
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
     CLOSE     SHTDENLU  CHUMONL1  SUBMEIL1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
