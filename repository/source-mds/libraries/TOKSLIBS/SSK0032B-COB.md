# SSK0032B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0032B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　出荷・受領マッチング処理　　　    *
*    作成日／更新日　　　：　2014/03/28                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　サカタ出荷実績とケーヨー受領実績の*
*                        ：　マッチング処理。　　　　　　　　　*
*            更新日　　　：　2014/04/11                        *
*                        ：　マッチングＫＥＹを変更　　　　　　*
*                        ：                                    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSK0032B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/03/28.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PRIMERGY6000.
 OBJECT-COMPUTER.       PRIMERGY6000.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 出荷実績抽出データ >>--*
     SELECT   KEISYKL2  ASSIGN         DA-01-VI-KEISYKL2
                        ORGANIZATION   INDEXED
                        ACCESS         MODE SEQUENTIAL
*↓2014.04.11 変更
*  店舗ＣＤ・伝票番号・行番号
*                       RECORD         KEY  JIS-F07
*                                           JIS-F02
*                                           JIS-F03
*  店舗ＣＤ・納品日・伝区ＣＤ（サカタ）・伝票番号・行番号
                        RECORD         KEY  JIS-F07
                                            JIS-F112
                                            JIS-F051
                                            JIS-F02
                                            JIS-F03
*↑2014.04.11
                        STATUS         JIS-ST.
*
*----<< 受領実績抽出データ >>--*
*↓2014.04.11 変更
*    SELECT   KEIJYWL2  ASSIGN         DA-01-VI-KEIJYWL2
     SELECT   KEIJYWL3  ASSIGN         DA-01-VI-KEIJYWL3
*↑2014.04.11
                        ORGANIZATION   INDEXED
                        ACCESS         MODE SEQUENTIAL
*↓2014.04.11 変更
*  店舗ＣＤ・伝票番号・行番号
*                       RECORD         KEY  JIJ-F04
*                                           JIJ-F05
*                                           JIJ-F06
*  店舗ＣＤ・納品予定日・伝区ＣＤ（サカタ）・伝票番号・行番号
                        RECORD         KEY  JIJ-F04
                                            JIJ-F09
                                            JIJ-F08
                                            JIJ-F05
                                            JIJ-F06
*↑2014.04.11
                        STATUS         JIJ-ST.
*
*----<< アンマッチデータ >>--*
     SELECT   KEIUMCF   ASSIGN         DA-01-VS-KEIUMCF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS         MODE SEQUENTIAL
                        STATUS         UNM-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*
*----<< 出荷実績抽出データ >>--*
 FD  KEISYKL2           LABEL RECORD   IS   STANDARD.
     COPY     KEISYKF   OF        XFDLIB
              JOINING   JIS       PREFIX.
*
*----<< 受領実績抽出データ >>--*
*↓2014.04.11 変更
*FD  KEIJYWL2           LABEL RECORD   IS   STANDARD.
 FD  KEIJYWL3           LABEL RECORD   IS   STANDARD.
*↑2014.04.11
     COPY     KEIJYWF   OF        XFDLIB
              JOINING   JIJ       PREFIX.
*
*----<< アンマッチデータ >>--*
 FD  KEIUMCF            LABEL RECORD   IS   STANDARD.
     COPY     KEIUMCF   OF        XFDLIB
              JOINING   UNM       PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  JIS-END        PIC  9(01)     VALUE  ZERO.
     03  JIJ-END        PIC  9(01)     VALUE  ZERO.
     03  INV-FLG        PIC  9(01)     VALUE  ZERO.
     03  CHK-FLG        PIC  X(01)     VALUE  SPACE.
     03  OUT-FLG        PIC  9(01)     VALUE  ZERO.
*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)     VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)     VALUE  ZERO.
     03  IN-CNT-JIS     PIC  9(09)     VALUE  ZERO.
     03  IN-CNT-JIJ     PIC  9(09)     VALUE  ZERO.
     03  CNT-7          PIC  9(06)     VALUE  ZERO.
     03  CNT-8          PIC  9(06)     VALUE  ZERO.
     03  CNT-9          PIC  9(06)     VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ファイルステータス >>--*
 01  JIS-ST             PIC  X(02).
 01  JIJ-ST             PIC  X(02).
 01  UNM-ST             PIC  X(02).
*
*----<< PG-ID    >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SSK0032B".
*
*----<< 日付ワーク >>--*
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
 01  SYS-DATEW          PIC 9(08).
*
*----<< 比較キー >>--*
*↓2014.04.11 変更
*01  BREAK-KEY.
*    03  JIS-KEY.
*        05  JIS-TENPO  PIC  9(05)    VALUE  ZERO.
*        05  JIS-DENNO  PIC  9(09)    VALUE  ZERO.
*        05  JIS-GYO    PIC  9(02)    VALUE  ZERO.
*    03  JIJ-KEY.
*        05  JIJ-TENPO  PIC  9(05)    VALUE  ZERO.
*        05  JIJ-DENNO  PIC  9(09)    VALUE  ZERO.
*        05  JIJ-GYO    PIC  9(02)    VALUE  ZERO.
 01  BREAK-KEY.
     03  JIS-KEY.
         05  JIS-TENPO  PIC  9(05)    VALUE  ZERO.
         05  JIS-NOUHIN PIC  9(08)    VALUE  ZERO.
         05  JIS-DENKU  PIC  9(02)    VALUE  ZERO.
         05  JIS-DENNO  PIC  9(09)    VALUE  ZERO.
         05  JIS-GYO    PIC  9(02)    VALUE  ZERO.
     03  JIJ-KEY.
         05  JIJ-TENPO  PIC  9(05)    VALUE  ZERO.
         05  JIJ-NOUHIN PIC  9(08)    VALUE  ZERO.
         05  JIJ-DENKU  PIC  X(02)    VALUE  ZERO.
         05  JIJ-DENNO  PIC  9(09)    VALUE  ZERO.
         05  JIJ-GYO    PIC  9(02)    VALUE  ZERO.
*↑2014.04.11
*
*----<< 件数カウントワーク >>--*
 01  CNT-AREA.
     03  CNT-UNMATCH    PIC  9(07)    VALUE  ZERO.
     03  CNT-OK         PIC  9(07)    VALUE  ZERO.
     03  CNT-JIS        PIC  9(07)    VALUE  ZERO.
     03  CNT-JIJ        PIC  9(07)    VALUE  ZERO.
*
*----<< メッセージ表示ワーク >>--*
 01  MSG-AREA.
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-PG          PIC   X(08)  VALUE "        ".
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
*
*----<< プログラム間連絡エリア >>--*
 01  PG-LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
*----<< ＣＬ連絡エリア >>--*
 LINKAGE                SECTION.
 01  PARA-BUMON         PIC   X(04).
 01  PARA-TANTOU        PIC   X(02).
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-BUMON
                                           PARA-TANTOU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*
*----<< 出荷実績抽出データ >>--*
 KEISYKL2-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEISYKL2.
     MOVE     PG-ID        TO   AB-PG.
     MOVE     "KEISYKL2"   TO   AB-FILE.
     MOVE     JIS-ST       TO   AB-STS.
     DISPLAY  MSG-ABEND         UPON CONS.
     DISPLAY  SEC-NAME          UPON CONS.
     DISPLAY  ABEND-FILE        UPON CONS.
     MOVE     4000         TO   PROGRAM-STATUS.
     STOP     RUN.
*
*----<< 受領実績抽出データ >>--*
*↓2014.04.11 変更
*KEIJYWL2-ERR             SECTION.
*    USE AFTER     EXCEPTION PROCEDURE      KEIJYWL2.
*    MOVE     PG-ID        TO   AB-PG.
*    MOVE     "KEIJYWL2"   TO   AB-FILE.
*    MOVE     JIJ-ST       TO   AB-STS.
*    DISPLAY  MSG-ABEND         UPON CONS.
*    DISPLAY  SEC-NAME          UPON CONS.
*    DISPLAY  ABEND-FILE        UPON CONS.
*    MOVE     4000           TO   PROGRAM-STATUS.
*    STOP     RUN.
 KEIJYWL3-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEIJYWL3.
     MOVE     PG-ID        TO   AB-PG.
     MOVE     "KEIJYWL3"   TO   AB-FILE.
     MOVE     JIJ-ST       TO   AB-STS.
     DISPLAY  MSG-ABEND         UPON CONS.
     DISPLAY  SEC-NAME          UPON CONS.
     DISPLAY  ABEND-FILE        UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*↑2014.04.11
*
*----<< アンマッチデータ >>--*
 KEIUMCF-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KEIUMCF.
     MOVE     PG-ID        TO   AB-PG.
     MOVE     "KEIUMCF "   TO   AB-FILE.
     MOVE     UNM-ST       TO   AB-STS.
     DISPLAY  MSG-ABEND         UPON CONS.
     DISPLAY  SEC-NAME          UPON CONS.
     DISPLAY  ABEND-FILE        UPON CONS.
     DISPLAY  "DEN-NO = " UNM-F02 UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END DECLARATIVES.
*
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
*
*プログラム開始メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
******************
*システム日付編集*
******************
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB" USING     LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*プログラムコントロール
     PERFORM  100-INIT-SEC.
     PERFORM  200-MAIN-SEC   UNTIL     JIS-END   =    1
                                   AND JIJ-END   =    1.
     PERFORM  300-END-SEC.
*プログラム終了メッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-SEC           SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*ファイルのオープン
     OPEN     INPUT     KEISYKL2.
*↓2014.04.11 変更
*    OPEN     INPUT     KEIJYWL2.
     OPEN     INPUT     KEIJYWL3.
*↑2014.04.11
     OPEN     OUTPUT    KEIUMCF.
*ワーク初期化
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     ZERO      TO   CNT-AREA.
     MOVE     ZERO      TO   JIS-END JIJ-END.
     INITIALIZE         BREAK-KEY.
*
*ファイル初期ＲＥＡＤ
     PERFORM  900-JIS-READ.
     PERFORM  900-JIJ-READ.
*
 100-INIT-SEC-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-SEC           SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
*マッチング処理
     EVALUATE  TRUE
*        出荷あり＆受領あり　（→値チェック）
         WHEN      JIS-KEY   =    JIJ-KEY
                   MOVE      "1"  TO      CHK-FLG
                   PERFORM   210-WRITE-SEC
                   PERFORM   900-JIS-READ
                   PERFORM   900-JIJ-READ
*        出荷あり＆受領なし
         WHEN      JIS-KEY   <    JIJ-KEY
                   MOVE      "2"  TO      CHK-FLG
                   PERFORM   210-WRITE-SEC
                   PERFORM   900-JIS-READ
*        出荷なし＆受領あり
         WHEN      JIS-KEY   >    JIJ-KEY
                   MOVE      "3"  TO      CHK-FLG
                   PERFORM   210-WRITE-SEC
                   PERFORM   900-JIJ-READ
     END-EVALUATE.
*
 200-MAIN-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-SEC            SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*
*ファイルのクローズ
     CLOSE    KEISYKL2.
*↓2014.04.11 変更
*    CLOSE    KEIJYWL2.
     CLOSE    KEIJYWL3.
*↑2014.04.11
     CLOSE    KEIUMCF.
*結果メッセージ
     DISPLAY NC"出荷データ読込件数＝"  IN-CNT-JIS  UPON CONS.
     DISPLAY NC"受領データ読込件数＝"  IN-CNT-JIJ  UPON CONS.
     DISPLAY NC"出荷データなし件数＝"  CNT-JIS     UPON CONS.
     DISPLAY NC"受領データなし件数＝"  CNT-JIJ     UPON CONS.
     DISPLAY NC"単価・数量違い件数＝"  CNT-UNMATCH UPON CONS.
     DISPLAY NC"ＯＫデータ　　件数＝"  CNT-OK      UPON CONS.
*
 300-END-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      データ出力　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 210-WRITE-SEC        SECTION.
*
     MOVE     "WRITE-SEC"         TO   S-NAME.
*
*アンマッチデータ出力
*****出荷あり＆受領あり
     IF  CHK-FLG  =  "1"
*        数量違い／原価違いチェック
         IF   ( JIS-F15  = JIJ-F12 ) AND
              ( JIS-F172 = JIJ-F13 )
*        ---->ＯＫ
              ADD      1              TO   CNT-OK
         ELSE
*        ---->ＮＧ
              MOVE     SPACE          TO   UNM-REC
              INITIALIZE                   UNM-REC
*             店舗ＣＤ
                MOVE     JIJ-F04        TO   UNM-F01
*             伝票番号
                MOVE     JIJ-F05        TO   UNM-F02
*             行番号
                MOVE     JIJ-F06        TO   UNM-F03
*             伝票区分（ケーヨー）
                MOVE     JIJ-F07        TO   UNM-F04
*             伝票区分（サカタ）
                MOVE     JIJ-F08        TO   UNM-F05
*             納品予定日
                MOVE     JIJ-F09        TO   UNM-F06
*             検収日
                MOVE     JIJ-F10        TO   UNM-F07
*             ＪＡＮＣＤ
                MOVE     JIJ-F11        TO   UNM-F08
*             検品数量
                MOVE     JIJ-F12        TO   UNM-F09
*             検品原価単価
                MOVE     JIJ-F13        TO   UNM-F10
*             出荷数
                MOVE     JIS-F15        TO   UNM-F11
*             納品日
                MOVE     JIS-F112       TO   UNM-F12
*             出荷原価単価
                MOVE     JIS-F172       TO   UNM-F13
*             メッセージ１
                MOVE     NC"レコード照合ＯＫ　　"
                                        TO   UNM-F14
*             メッセージ２
*      　      _数量・原価ともに不一致の場合
                IF     ( JIS-F15  NOT = JIJ-F12 ) AND
                       ( JIS-F172 NOT = JIJ-F13 )
                         MOVE     NC"不一致＞数量　原価　"
                                        TO   UNM-F15
                END-IF
*        　    _数量のみ不一致の場合
                IF     ( JIS-F15  NOT = JIJ-F12 ) AND
                       ( JIS-F172     = JIJ-F13 )
                         MOVE     NC"不一致＞数量　　　　"
                                        TO   UNM-F15
                END-IF
*     　 　    _原価のみ不一致の場合
                IF     ( JIS-F15      = JIJ-F12 ) AND
                       ( JIS-F172 NOT = JIJ-F13 )
                         MOVE     NC"不一致＞　　　原価　"
                                        TO   UNM-F15
                END-IF
*             登録日付
                MOVE     SYS-DATEW      TO   UNM-F96
*             登録時刻
                MOVE     SYS-TIME(1:6)  TO   UNM-F97
*             登録部門
                MOVE     PARA-BUMON     TO   UNM-F98
*             登録担当者
                MOVE     PARA-TANTOU    TO   UNM-F99
*
              WRITE  UNM-REC
              ADD       1             TO   CNT-UNMATCH
         END-IF
     END-IF.
*
*****出荷あり＆受領なし
     IF  CHK-FLG  =  "2"
         MOVE     SPACE          TO   UNM-REC
         INITIALIZE                   UNM-REC
*        店舗ＣＤ
           MOVE     JIS-F07        TO   UNM-F01
*        伝票番号
           MOVE     JIS-F02        TO   UNM-F02
*        行番号
           MOVE     JIS-F03        TO   UNM-F03
*        伝票区分（ケーヨー）
           MOVE     "  "           TO   UNM-F04
*        伝票区分（サカタ）
           MOVE     JIS-F051       TO   UNM-F05
*        納品予定日
           MOVE     JIS-F112       TO   UNM-F06
*        検収日
           MOVE     ZERO           TO   UNM-F07
*        ＪＡＮＣＤ
           MOVE     JIS-F25        TO   UNM-F08
*        検品数量
           MOVE     ZERO           TO   UNM-F09
*        検品原価単価
           MOVE     ZERO           TO   UNM-F10
*        出荷数
           MOVE     JIS-F15        TO   UNM-F11
*        納品日
           MOVE     JIS-F112       TO   UNM-F12
*        出荷原価単価
           MOVE     JIS-F172       TO   UNM-F13
*        メッセージ１
           MOVE     NC"受領データなし　　　" TO   UNM-F14
*        メッセージ２
           MOVE     NC"　　　　　　　　　　" TO   UNM-F15
*        登録日付
           MOVE     SYS-DATEW      TO   UNM-F96
*        登録時刻
           MOVE     SYS-TIME(1:6)  TO   UNM-F97
*        登録部門
           MOVE     PARA-BUMON     TO   UNM-F98
*        登録担当者
           MOVE     PARA-TANTOU    TO   UNM-F99
*
         WRITE  UNM-REC
         ADD       1             TO   CNT-JIJ
     END-IF.
*
*****出荷なし＆受領あり
     IF  CHK-FLG  =  "3"
         MOVE     SPACE          TO   UNM-REC
         INITIALIZE                   UNM-REC
*        店舗ＣＤ
           MOVE     JIJ-F04        TO   UNM-F01
*        伝票番号
           MOVE     JIJ-F05        TO   UNM-F02
*        行番号
           MOVE     JIJ-F06        TO   UNM-F03
*        伝票区分（ケーヨー）
           MOVE     JIJ-F07        TO   UNM-F04
*        伝票区分（サカタ）
           MOVE     JIJ-F08        TO   UNM-F05
*        納品予定日
           MOVE     JIJ-F09        TO   UNM-F06
*        検収日
           MOVE     JIJ-F10        TO   UNM-F07
*        ＪＡＮＣＤ
           MOVE     JIJ-F11        TO   UNM-F08
*        検品数量
           MOVE     JIJ-F12        TO   UNM-F09
*        検品原価単価
           MOVE     JIJ-F13        TO   UNM-F10
*        出荷数
           MOVE     ZERO           TO   UNM-F11
*        納品日
           MOVE     ZERO           TO   UNM-F12
*        出荷原価単価
           MOVE     ZERO           TO   UNM-F13
*        メッセージ１
           MOVE     NC"出荷データなし　　　" TO   UNM-F14
*        メッセージ２
           MOVE     NC"　　　　　　　　　　" TO   UNM-F15
*        登録日付
           MOVE     SYS-DATEW      TO   UNM-F96
*        登録時刻
           MOVE     SYS-TIME(1:6)  TO   UNM-F97
*        登録部門
           MOVE     PARA-BUMON     TO   UNM-F98
*        登録担当者
           MOVE     PARA-TANTOU    TO   UNM-F99
*
         WRITE  UNM-REC
         ADD       1             TO   CNT-JIS
     END-IF.
*
 210-WRITE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    受領実績Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-JIJ-READ           SECTION.
*
     MOVE     "JIJ-READ  "        TO   S-NAME.
*
     IF       JIJ-END   =   1
              GO        TO        900-JIJ-READ-EXIT
     END-IF.
*
*↓2014.04.11 変更
*    READ     KEIJYWL2
     READ     KEIJYWL3
*↑2014.04.11
               AT  END
                   MOVE   HIGH-VALUE   TO   JIJ-KEY
                   MOVE      1         TO   JIJ-END
               NOT AT END
                   MOVE      JIJ-F04   TO   JIJ-TENPO
*↓2014.04.11 追加
                   MOVE      JIJ-F09   TO   JIJ-NOUHIN
                   MOVE      JIJ-F08   TO   JIJ-DENKU
*↑2014.04.11
                   MOVE      JIJ-F05   TO   JIJ-DENNO
                   MOVE      JIJ-F06   TO   JIJ-GYO
                   ADD       1         TO   IN-CNT-JIJ
     END-READ.
*
 900-JIJ-READ-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    LEVEL ALL    出荷実績ファイル　 READ                      *
*--------------------------------------------------------------*
 900-JIS-READ           SECTION.
*
     MOVE     "JIS-READ  "        TO   S-NAME.
*
     IF       JIS-END   =   1
              GO        TO        900-JIS-READ-EXIT
     END-IF.
*
     READ     KEISYKL2
         AT END
              MOVE      HIGH-VALUE     TO   JIS-KEY
              MOVE      1              TO   JIS-END
         NOT AT END
              MOVE      JIS-F07   TO   JIS-TENPO
*↓2014.04.11 追加
              MOVE      JIS-F112  TO   JIS-NOUHIN
              MOVE      JIS-F051  TO   JIS-DENKU
*↑2014.04.11
              MOVE      JIS-F02   TO   JIS-DENNO
              MOVE      JIS-F03   TO   JIS-GYO
              ADD       1         TO   IN-CNT-JIS
     END-READ.
*
 900-JIS-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
