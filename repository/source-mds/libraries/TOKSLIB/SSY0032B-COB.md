# SSY0032B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY0032B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　出荷明細データ抽出処理            *
*    作成日／更新日　　　：　99/09/13                          *
*    作成者／更新者　　　：　ＮＡＶ吉田　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、該当    *
*                            のデータを売上伝票データファイル  *
*                            より抽出する。                    *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　　更新日／更新者　　　：　04/07/01  /  YOSHIDA              *
*                        ：　11/10/05  /  YOSHIDA.M            *
*    修正概要　　　　　　：　基幹サーバ統合                    *
*　                                                            *
*　　更新日／更新者　　　：　2013/12/11/  NAV TAKAHASHI (*$$)  *
*    修正概要　　　　　　：　消費税増税対応                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY0032B.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          04/07/02.
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
***2011.10.05(DEN-F07,DEN-F112)
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*出荷明細ワークファイル
     SELECT   SHWSYUKF  ASSIGN    TO        DA-01-S-SHWSYUKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SYU-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
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
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    出荷明細ワークファイル
******************************************************************
 FD  SHWSYUKF           BLOCK     CONTAINS  5    RECORDS.
     COPY     SHWSYUKF  OF        XFDLIB
              JOINING   SYU       PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*****************************************************************
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SYU-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY0032B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0032B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY0032B".
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
*税計算用
*$$2013/12/11 NAV ST  消費税増税対応
*01  WK-ZEIRITU              PIC  9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU1             PIC  9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU2             PIC  9(01)V99  VALUE  ZERO.
 01  WK-ZEIKAITEI            PIC  9(08)     VALUE  ZERO.
 01  WK-ZEIRITU3             PIC  9(01)V99  VALUE  ZERO.
 01  WK-ZEIRITU4             PIC  9(01)V99  VALUE  ZERO.
*$$2013/12/11 NAV END 消費税増税対応
 01  WK-ZEIKOMI              PIC  9(09)     VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-OUTNO             PIC   9(01).
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUHIN            PIC   9(08).
 01  PARA-STEN              PIC   9(05).
 01  PARA-ETEN              PIC   9(05).
 01  PARA-SBUMON            PIC   X(04).
 01  PARA-EBUMON            PIC   X(04).
 01  PARA-STANA             PIC   X(06).
 01  PARA-ETANA             PIC   X(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-OUTNO
                                       PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUHIN
                                       PARA-STEN
                                       PARA-ETEN
                                       PARA-SBUMON
                                       PARA-EBUMON
                                       PARA-STANA
                                       PARA-ETANA.
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
                        PROCEDURE   SHWSYUKF.
     MOVE      "SHWSYUKF"   TO   AB-FILE.
     MOVE      SYU-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
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
 FILEERR-SEC4           SECTION.
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
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOKEN1"    TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SHTDENLA  SHOTBL1   TOKMS2    HJYOKEN.
     OPEN     OUTPUT    SHWSYUKF.
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
*    得意先マスタ検索
     MOVE     SPACE          TO   TOK-REC
     INITIALIZE                   TOK-REC
     MOVE     PARA-TORICD    TO   TOK-F01
     READ     TOKMS2
         INVALID
              MOVE SPACE     TO   TOK-REC
              INITIALIZE          TOK-REC
     END-READ.
*    条件ファイル検索
     MOVE     SPACE          TO   JYO-REC
     INITIALIZE                   JYO-REC
     MOVE     "99"           TO   JYO-F01.
     MOVE     "ZEI"          TO   JYO-F02.
     READ     HJYOKEN
         INVALID
*$$2013/12/11 NAV ST  消費税増税対応
*                     消費税税率が取得出来ない場合は異常終了
**************MOVE ZERO      TO   WK-ZEIRITU1
              DISPLAY NC"消費税率取得エラー！！" UPON CONS
              MOVE 4000      TO   PROGRAM-STATUS
              STOP  RUN
*$$2013/12/11 NAV ED  消費税増税対応
         NOT  INVALID
*$$2013/12/11 NAV ST  消費税増税対応
*                     項目変更
              MOVE JYO-F04   TO   WK-ZEIRITU1
              MOVE JYO-F05   TO   WK-ZEIRITU2
              MOVE JYO-F06   TO   WK-ZEIKAITEI
              MOVE JYO-F07   TO   WK-ZEIRITU3
              MOVE JYO-F08   TO   WK-ZEIRITU4
*$$2013/12/11 NAV ED  消費税増税対応
     END-READ.
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TORICD    TO   DEN-F01.
     MOVE     PARA-SOKO      TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
***2011.10.05(DEN-F07,DEN-F112)
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
            ( PARA-TORICD    =    DEN-F01 )
              IF   PARA-SOKO      NOT =     SPACE
**************AND  PARA-SOKO      NOT =     "00"
                   IF   PARA-SOKO      =    DEN-F48
                        CONTINUE
                   ELSE
                        GO   TO   MAIN-010
                   END-IF
              END-IF
     ELSE
              MOVE      9         TO   END-FG
              GO        TO        MAIN-EXIT
     END-IF.
*    店舗コード
     IF     ( PARA-STEN      <=   DEN-F07   ) AND
            ( DEN-F07        <=   PARA-ETEN )
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
*    納品日
     IF       PARA-NOUHIN    NOT =     ZERO
         IF   PARA-NOUHIN    =    DEN-F112
              CONTINUE
         ELSE
              GO        TO        MAIN-010
         END-IF
     ELSE
              CONTINUE
     END-IF.
*    部門コード
     IF     ( PARA-SBUMON    <=   DEN-F12     ) AND
            ( DEN-F12        <=   PARA-EBUMON )
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
*    _番
     IF     ( PARA-STANA     <=   DEN-F49     ) AND
            ( DEN-F49        <=   PARA-ETANA  )
              CONTINUE
     ELSE
              GO        TO        MAIN-010
     END-IF.
*
*出荷明細ワーク出力
     MOVE     SPACE          TO   SYU-REC.
     INITIALIZE                   SYU-REC.
     MOVE     DEN-F46        TO   SYU-F011.
     MOVE     DEN-F47        TO   SYU-F012.
     MOVE     DEN-F01        TO   SYU-F013.
     MOVE     DEN-F48        TO   SYU-F02.
     MOVE     DEN-F07        TO   SYU-F03.
     MOVE     DEN-F12        TO   SYU-F04.
*2000/03/14 ST NAV
*  商品変換テーブル検索
     MOVE     DEN-F01        TO   TBL-F01.
     MOVE     DEN-F25        TO   TBL-F02.
     READ     SHOTBL1
       INVALID
              MOVE  SPACE    TO   SYU-F05
       NOT INVALID
              MOVE  TBL-F08  TO   SYU-F05
     END-READ.
*****MOVE     DEN-F49        TO   SYU-F05.
*2000/03/14 ED NAV
     MOVE     DEN-F1411      TO   SYU-F061.
     MOVE     DEN-F1412      TO   SYU-F062.
     MOVE     DEN-F25        TO   SYU-F07.
     MOVE     DEN-F1421      TO   SYU-F081.
     MOVE     DEN-F1422      TO   SYU-F082.
     MOVE     DEN-F50        TO   SYU-F09.
     MOVE     DEN-F15        TO   SYU-F10.
     MOVE     DEN-F172       TO   SYU-F11.
     MOVE     DEN-F173       TO   SYU-F12.
     MOVE     SPACE          TO   SYU-F13.
     MOVE     SPACE          TO   SYU-F14.
     MOVE     DEN-F112       TO   SYU-F15.
*### 1999/11/22 NAV T.T START ###*
     MOVE     DEN-F02        TO   SYU-F16.
     MOVE     DEN-F27D       TO   SYU-F17.
*### 1999/11/22 NAV T.T START ###*
**
     IF  TOK-F97  =  "0"
*&&2013/12/11 NAV ST 消費税税増税対応
******** EVALUATE  TOK-F88
*            WHEN  "0"
*            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1
*            WHEN  "4"
*            COMPUTE WK-ZEIKOMI ROUNDED = DEN-F173 * WK-ZEIRITU1
*            WHEN  "9"
*            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1 + 0.99
*            WHEN  OTHER
*            CONTINUE
*********END-EVALUATE
*******消費税改定日が指定された場合
       IF  WK-ZEIKAITEI NOT =  ZERO
********消費税改定日以上の場合、新消費税率を使用
        IF  DEN-F112 >= WK-ZEIKAITEI
            EVALUATE  TOK-F88
            WHEN  "0"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU3
            WHEN  "4"
            COMPUTE WK-ZEIKOMI ROUNDED = DEN-F173 * WK-ZEIRITU3
            WHEN  "9"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU3 + 0.9
            WHEN  OTHER
            COMPUTE WK-ZEIKOMI = DEN-F173
            END-EVALUATE
            MOVE        WK-ZEIKOMI   TO         SYU-F18
        ELSE
************消費税改定日以下の場合、旧消費税率を使用
            EVALUATE  TOK-F88
            WHEN  "0"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1
            WHEN  "4"
            COMPUTE WK-ZEIKOMI ROUNDED = DEN-F173 * WK-ZEIRITU1
            WHEN  "9"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1 + 0.9
            WHEN  OTHER
            COMPUTE WK-ZEIKOMI = DEN-F173
            END-EVALUATE
            MOVE        WK-ZEIKOMI   TO         SYU-F18
        END-IF
       ELSE
************消費税改定日が指定されていない場合
            EVALUATE  TOK-F88
            WHEN  "0"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1
            WHEN  "4"
            COMPUTE WK-ZEIKOMI ROUNDED = DEN-F173 * WK-ZEIRITU1
            WHEN  "9"
            COMPUTE WK-ZEIKOMI = DEN-F173 * WK-ZEIRITU1 + 0.9
            WHEN  OTHER
            COMPUTE WK-ZEIKOMI = DEN-F173
            END-EVALUATE
            MOVE        WK-ZEIKOMI   TO         SYU-F18
       END-IF
     END-IF.
*&&2013/12/11 NAV ED 消費税税増税対応
**
     WRITE    SYU-REC.
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
     CLOSE     SHTDENLA  SHWSYUKF  SHOTBL1  TOKMS2  HJYOKEN.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
