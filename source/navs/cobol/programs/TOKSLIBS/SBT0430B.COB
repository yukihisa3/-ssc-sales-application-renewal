****************************************************************
*    顧客名　　　　　　　：　_サカタのタネ殿　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携（カインズ）        *
*    モジュール名　　　　：　カインズ出荷梱包ＭＳＧ　　　　    *
*    　　　　　　　　　　：　　→　チェック用ファイル編集　    *
*    作成日　　　　　　　：　2014/08/15                        *
*    作成者　　　　　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷梱包（取引明細）と（欠品情報）*
*                        ：　よりチェック用データを作成する。　*
*    更新日　　　　　　　：　2014/09/05,08                     *
*    更新者　　　　　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    変更概要　　　　　　：　・全欠品（欠品情報のみ）対応　　　*
*                        ：　・別梱包（同一取引明細）対応　　　*
*                        ：                                    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBT0430B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/08/15.
 DATE-COMPILED.
*SECURITY.              NONE.
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
*----<< 出荷梱包（取引明細） >>--*
     SELECT   SYKMEF    ASSIGN         DA-01-S-SYKMEF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS         MODE SEQUENTIAL
                        STATUS         MEF-ST.
*
*----<< 出荷梱包（欠品情報） >>--*
     SELECT   SYKKPF    ASSIGN         DA-01-S-SYKKPF
                        ORGANIZATION   SEQUENTIAL
                        ACCESS         MODE SEQUENTIAL
                        STATUS         KPF-ST.
*
*2014/09/08↓
*----<< 出荷梱包チェックワーク >>--*
*    SELECT   SYKCKF    ASSIGN         DA-01-VS-SYKCKF
*                       ORGANIZATION   SEQUENTIAL
*                       ACCESS         MODE SEQUENTIAL
*                       STATUS         CKF-ST.
*----<< 出荷梱包チェックワーク >>--*   検索用
     SELECT   SYKCKL3   ASSIGN         DA-01-VI-SYKCKL3
                        ORGANIZATION   INDEXED
                        ACCESS         MODE DYNAMIC
                        RECORD    KEY  CKF-F013
                                       CKF-F014
                                       CKF-F0205
                                       CKF-F0202
                                       CKF-F0203
*                       WITH           DUPLICATES
                        STATUS         CKF-ST.
*2014/09/08↑
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*
*----<< 出荷梱包（取引明細） >>--*
 FD  SYKMEF             BLOCK CONTAINS  1   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     SYKMEF    OF    XFDLIB
              JOINING   MEF   PREFIX.
*
*----<< 出荷梱包（欠品情報） >>--*
 FD  SYKKPF             BLOCK CONTAINS  1   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     SYKKPF    OF    XFDLIB
              JOINING   KPF   PREFIX.
*
*2014/09/08↓
*----<< 出荷梱包チェックワーク >>--*
*FD  SYKCKF             LABEL RECORD   IS   STANDARD.
*    COPY     SYKCKF    OF    XFDLIB
*             JOINING   CKF   PREFIX.
*----<< 出荷梱包チェックワーク >>--*
 FD  SYKCKL3            LABEL RECORD   IS   STANDARD.
     COPY     SYKCKF    OF    XFDLIB
              JOINING   CKF   PREFIX.
*2014/09/08↑
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  MEF-END        PIC  9(01)     VALUE  ZERO.
     03  KPF-END        PIC  9(01)     VALUE  ZERO.
     03  INV-FLG        PIC  9(01)     VALUE  ZERO.
     03  CHK-FLG        PIC  X(01)     VALUE  SPACE.
     03  OUT-FLG        PIC  9(01)     VALUE  ZERO.
*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)     VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)     VALUE  ZERO.
     03  IN-CNT-MEF     PIC  9(08)     VALUE  ZERO.
     03  IN-CNT-KPF     PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT-SYKCKF PIC  9(08)     VALUE  ZERO.
*2014/09/08↓
     03  UPD-CNT-SYKCKF PIC  9(08)     VALUE  ZERO.
*2014/09/08↑
     03  CNT-7          PIC  9(06)     VALUE  ZERO.
     03  CNT-8          PIC  9(06)     VALUE  ZERO.
     03  CNT-9          PIC  9(06)     VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ファイルステータス >>--*
 01  MEF-ST             PIC  X(02).
 01  KPF-ST             PIC  X(02).
 01  CKF-ST             PIC  X(02).
*
*----<< PG-ID    >>--*
 01  PG-ID              PIC  X(08)     VALUE   "SBT0430B".
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
 01  BREAK-KEY.
     03  MEF-KEY.
         05  MEF-NOUHIN PIC  X(08)    VALUE  SPACE.
         05  MEF-DENNO  PIC  X(10)    VALUE  SPACE.
         05  MEF-GYO    PIC  X(04)    VALUE  SPACE.
     03  KPF-KEY.
         05  KPF-NOUHIN PIC  X(08)    VALUE  SPACE.
         05  KPF-DENNO  PIC  X(10)    VALUE  SPACE.
         05  KPF-GYO    PIC  X(04)    VALUE  SPACE.
*
*----<< 件数カウントワーク >>--*
 01  CNT-AREA.
     03  CNT-OK         PIC  9(07)    VALUE  ZERO.
     03  CNT-MEF        PIC  9(07)    VALUE  ZERO.
     03  CNT-KPF        PIC  9(07)    VALUE  ZERO.
*
*----<< 計算ワーク >>--*
 01  SUM-AREA.
     03  WK-MEF-F26.
       05  NUM-MEF-F26      PIC  9(07)   VALUE  ZERO.
     03  WK-CKF-F0226.
       05  NUM-CKF-F0226    PIC  9(07)   VALUE  ZERO.
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
 LINKAGE                    SECTION.
 01  PARA-TOKCD             PIC   X(08).
 01  PARA-SOKCD             PIC   X(02).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-BUMCD             PIC   X(04).
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-TOKCD
                                           PARA-SOKCD
                                           PARA-TANCD
                                           PARA-BUMCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*
*----<< 出荷梱包（取引明細）>>--*
 SYKMEF-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYKMEF.
     MOVE     PG-ID        TO   AB-PG.
     MOVE     "SYKMEF"     TO   AB-FILE.
     MOVE     MEF-ST       TO   AB-STS.
     DISPLAY  MSG-ABEND         UPON CONS.
     DISPLAY  SEC-NAME          UPON CONS.
     DISPLAY  ABEND-FILE        UPON CONS.
     MOVE     4000         TO   PROGRAM-STATUS.
     STOP     RUN.
*
*----<< 出荷梱包（欠品情報）>>--*
 SYKKPF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYKKPF.
     MOVE     PG-ID        TO   AB-PG.
     MOVE     "SYKKPF"     TO   AB-FILE.
     MOVE     KPF-ST       TO   AB-STS.
     DISPLAY  MSG-ABEND         UPON CONS.
     DISPLAY  SEC-NAME          UPON CONS.
     DISPLAY  ABEND-FILE        UPON CONS.
     MOVE     4000         TO   PROGRAM-STATUS.
     STOP     RUN.
*
*2014/09/08↓
*----<< 出荷梱包チェックワーク >>--*
*SYKCKF-ERR              SECTION.
*    USE AFTER     EXCEPTION PROCEDURE      SYKCKF.
*    MOVE     PG-ID        TO   AB-PG.
*    MOVE     "SYKCKF "    TO   AB-FILE.
*    MOVE     CKF-ST       TO   AB-STS.
*    DISPLAY  MSG-ABEND         UPON CONS.
*    DISPLAY  SEC-NAME          UPON CONS.
*    DISPLAY  ABEND-FILE        UPON CONS.
*    MOVE     4000         TO   PROGRAM-STATUS.
*    STOP     RUN.
*----<< 出荷梱包チェックワーク >>--*
 SYKCK3-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYKCKL3.
     MOVE     PG-ID        TO   AB-PG.
     MOVE     "SYKCKL3"    TO   AB-FILE.
     MOVE     CKF-ST       TO   AB-STS.
     DISPLAY  MSG-ABEND         UPON CONS.
     DISPLAY  SEC-NAME          UPON CONS.
     DISPLAY  ABEND-FILE        UPON CONS.
     MOVE     4000         TO   PROGRAM-STATUS.
     STOP     RUN.
*2014/09/08↑
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
     PERFORM  200-MAIN-SEC   UNTIL     MEF-END   =    1
                                   AND KPF-END   =    1.
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
     OPEN     INPUT     SYKMEF.
     OPEN     INPUT     SYKKPF.
*2014/09/08↓
*    OPEN     OUTPUT    SYKCKF.
     OPEN     I-O       SYKCKL3.
*2014/09/08↑
*ワーク初期化
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     ZERO      TO   CNT-AREA.
     MOVE     ZERO      TO   MEF-END KPF-END.
     INITIALIZE         BREAK-KEY.
*
*ファイル初期ＲＥＡＤ
     PERFORM  900-MEF-READ.
     PERFORM  900-KPF-READ.
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
*###     DISPLAY "MEF-KEY = " MEF-KEY UPON CONS.
*###     DISPLAY "KPF-KEY = " KPF-KEY UPON CONS.
     EVALUATE  TRUE
*        明細あり＆欠品あり　→ファイル出力　
         WHEN      MEF-KEY   =    KPF-KEY
                   MOVE      "1"  TO      CHK-FLG
                   PERFORM   210-WRITE-SEC
                   PERFORM   900-MEF-READ
                   PERFORM   900-KPF-READ
*        明細あり＆欠品なし　→ファイル出力
         WHEN      MEF-KEY   <    KPF-KEY
                   MOVE      "2"  TO      CHK-FLG
                   PERFORM   210-WRITE-SEC
                   PERFORM   900-MEF-READ
*2014.09.05↓
*        明細なし＆欠品あり　＝ファイル出力（取引明細編集）
         WHEN      MEF-KEY   >    KPF-KEY
                   MOVE      "3"  TO      CHK-FLG
                   PERFORM   210-WRITE-SEC
                   PERFORM   900-KPF-READ
*        明細なし＆欠品あり　＝データ異常　→エラー終了
*        WHEN      MEF-KEY   >    KPF-KEY
*            DISPLAY
*             NC"！！！！！！！！！！！！！！！！"    UPON CONS
*            DISPLAY
*             NC"　明細なし欠品情報が存在します　"    UPON CONS
*            DISPLAY
*             NC"　　【欠品情報" IN-CNT-KPF NC"件目】　　"
*                                                     UPON CONS
*            DISPLAY
*             NC"　　【納品日　＝" KPF-F08 "  "     NC"】　　"
*                                                     UPON CONS
*            DISPLAY
*             NC"　　【伝票番号＝" KPF-F05          NC"】　　"
*                                                     UPON CONS
*            DISPLAY
*             NC"　　【行番号　＝" KPF-F06 "      " NC"】　　"
*                                                     UPON CONS
*            DISPLAY
*             NC"　処理を中止します　　　　　　　"    UPON CONS
*            DISPLAY
*             NC"！！！！！！！！！！！！！！！！"    UPON CONS
*            MOVE    4000         TO   PROGRAM-STATUS
*            STOP    RUN
*2014.09.05↑
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
     CLOSE    SYKMEF.
     CLOSE    SYKKPF.
*2014/09/08↓
*    CLOSE    SYKCKF.
     CLOSE    SYKCKL3.
*2014/09/08↑
*結果メッセージ
     DISPLAY NC"明細データ件数＝"  IN-CNT-MEF      UPON CONS.
     DISPLAY NC"欠品データ件数＝"  IN-CNT-KPF      UPON CONS.
     DISPLAY NC"データ出力件数＝"  OUT-CNT-SYKCKF  UPON CONS.
     DISPLAY NC"データ更新件数＝"  UPD-CNT-SYKCKF  UPON CONS.
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
*出荷梱包チェックワーク出力
     MOVE        SPACE          TO   CKF-REC.
     INITIALIZE                      CKF-REC.
*   【共通部】
*     取込日
        MOVE     SYS-DATEW      TO   CKF-F011.
*     取込時刻
        MOVE     SYS-TIME(1:4)  TO   CKF-F012.
*     取引先コード
        MOVE     PARA-TOKCD     TO   CKF-F013.
*     出荷場所
        MOVE     PARA-SOKCD     TO   CKF-F014.
*     エラー区分
*       後続処理(SBT0440B)にてセットする為ここでは非セット
        MOVE     SPACE          TO   CKF-F015.
*     取込者部門ＣＤ
        MOVE     PARA-BUMCD     TO   CKF-F016.
*     取込担当者ＣＤ
        MOVE     PARA-TANCD     TO   CKF-F017.
*
*****明細あり＆欠品あり時
     IF  CHK-FLG  =  "1"
*2014/09/08↓
*      チェックファイル存在チェック
*      　存在時、出荷数量（バラ）を加算するため
           MOVE     0              TO   INV-FLG
           MOVE     MEF-F02        TO   CKF-F0202
           MOVE     MEF-F03        TO   CKF-F0203
           MOVE     MEF-F05        TO   CKF-F0205
           READ     SYKCKL3
               INVALID
                    MOVE  1        TO   INV-FLG
           END-READ
*2014/09/08↑
*      【取引明細部】
*2014/09/08↓
*     　チェックファイル非存在時→レコードセット
*     　　　　　　　　　　存在時→出荷数量（バラ）を加算
        IF       INV-FLG   =    1
*           レコード
                 MOVE     MEF-REC(1:223) TO   CKF-F02
*           出荷指示数
*           後続処理(SBT0440B)にてセットする為ここでは非セット
                 MOVE     ZERO           TO   CKF-F0231
*           出荷数量（発注単位数）
                 MOVE     ALL "0"        TO   CKF-F0227
        ELSE
*           出荷数量（バラ）加算
                 MOVE     MEF-REC(178:7) TO   WK-MEF-F26
                 MOVE     CKF-F0226      TO   WK-CKF-F0226
                 ADD      NUM-MEF-F26    TO   NUM-CKF-F0226
*                MOVE     MEF-REC(1:223) TO   CKF-F02
                 MOVE     WK-CKF-F0226   TO   CKF-F0226
        END-IF
*2014/09/08↑
*
*      【欠品情報部】
*         レコード
          MOVE   KPF-REC(1:178) TO   CKF-F03
*         欠品数量（発注単位数）
          MOVE   ALL "0"        TO   CKF-F0303
*
*2014/09/08↓
        IF       INV-FLG  =    1
                 WRITE    CKF-REC
                 ADD      1              TO   OUT-CNT-SYKCKF
        ELSE
                 REWRITE  CKF-REC
                 ADD      1              TO   UPD-CNT-SYKCKF
        END-IF
*2014/09/08↑
     END-IF.
*
*****明細あり＆欠品なし時
     IF  CHK-FLG  =  "2"
*2014/09/08↓
*      チェックファイル存在チェック
*      　存在時、出荷数量（バラ）を加算するため
           MOVE     0              TO   INV-FLG
           MOVE     MEF-F02        TO   CKF-F0202
           MOVE     MEF-F03        TO   CKF-F0203
           MOVE     MEF-F05        TO   CKF-F0205
           READ     SYKCKL3
               INVALID
                    MOVE  1        TO   INV-FLG
           END-READ
*2014/09/08↑
*      【取引明細部】
*2014/09/08↓
*     　チェックファイル非存在時→レコードセット
*     　　　　　　　　　　存在時→出荷数量（バラ）を加算
        IF       INV-FLG   =    1
*           レコード
                 MOVE     MEF-REC(1:223) TO   CKF-F02
*           出荷指示数
*           後続処理(SBT0440B)にてセットする為ここでは非セット
                 MOVE     ZERO           TO   CKF-F0231
*           出荷数量（発注単位数）
                 MOVE     ALL "0"        TO   CKF-F0227
        ELSE
*           出荷数量（バラ）加算
                 MOVE     MEF-REC(178:7) TO   WK-MEF-F26
                 MOVE     CKF-F0226      TO   WK-CKF-F0226
                 ADD      NUM-MEF-F26    TO   NUM-CKF-F0226
*                MOVE     MEF-REC(1:223) TO   CKF-F02
                 MOVE     WK-CKF-F0226   TO   CKF-F0226
        END-IF
*2014/09/08↑
*
*      【欠品情報部】
*        レコード
*          欠品情報レコードは存在しないため非セット
*
*2014/09/08↓
        IF       INV-FLG  =    1
                 WRITE    CKF-REC
                 ADD      1              TO   OUT-CNT-SYKCKF
        ELSE
                 REWRITE  CKF-REC
                 ADD      1              TO   UPD-CNT-SYKCKF
        END-IF
*2014/09/08↑
     END-IF.
*
*2014.09.05↓
*****明細なし＆欠品あり時
     IF  CHK-FLG  =  "3"
*      【取引明細部】
*        欠品情報よりセット
*        レコード区分
           MOVE     "E"            TO   CKF-F0201
*        取引番号
           MOVE     KPF-F05        TO   CKF-F0202
*        明細番号
           MOVE     KPF-F06        TO   CKF-F0203
*        発注日
           MOVE     KPF-F07        TO   CKF-F0204
*        最終納品先納品日
           MOVE     KPF-F08        TO   CKF-F0205
*        商品コード（ＧＴＩＮ）
           MOVE     KPF-F09        TO   CKF-F0206
*        商品コード（発注用）
           MOVE     KPF-F10        TO   CKF-F0207
*        商品コード区分
           MOVE     KPF-F11        TO   CKF-F0208
*        商品コード（取引先）
           MOVE     KPF-F12        TO   CKF-F0209
*        商品分類（大）
           MOVE     KPF-F13        TO   CKF-F0210
*        商品分類（中）
           MOVE     KPF-F14        TO   CKF-F0211
*        商品分類（小）
           MOVE     KPF-F15        TO   CKF-F0212
*        商品分類（細）
           MOVE     KPF-F16        TO   CKF-F0213
*        賞味期限
           MOVE     SPACE          TO   CKF-F0214
*        製造日
           MOVE     SPACE          TO   CKF-F0215
*        製造番号
           MOVE     SPACE          TO   CKF-F0216
*        商品区分
           MOVE     KPF-F17        TO   CKF-F0217
*        発注区分
           MOVE     KPF-F18        TO   CKF-F0218
*        EOS区分
           MOVE     KPF-F19        TO   CKF-F0219
*        処理種別
           MOVE     KPF-F20        TO   CKF-F0220
*        商材区分
           MOVE     SPACE          TO   CKF-F0221
*        発注単位
           MOVE     KPF-F21        TO   CKF-F0222
*        発注単位コード
           MOVE     KPF-F22        TO   CKF-F0223
*        発注数量（バラ）
           MOVE     KPF-F23        TO   CKF-F0224
*        発注数量（発注単位）
           MOVE     KPF-F24        TO   CKF-F0225
*        出荷数量（バラ）
           MOVE     ALL "0"        TO   CKF-F0226
*        出荷数量（発注単位数）
           MOVE     ALL "0"        TO   CKF-F0227
*        原単価
           MOVE     KPF-F25        TO   CKF-F0228
*        売単価
           MOVE     KPF-F26        TO   CKF-F0229
*        最終納品先ＣＤ
*          後続処理(SBT0440B)にてセットする為ここでは非セット
           MOVE     "00000"        TO   CKF-F0230
*        出荷指示数
*          後続処理(SBT0440B)にてセットする為ここでは非セット
           MOVE     ZERO           TO   CKF-F0231
*
*      【欠品情報部】
*        レコード
           MOVE     KPF-REC(1:178) TO   CKF-F03
*         欠品数量（発注単位数）
           MOVE     ALL "0"        TO   CKF-F0303
*
           WRITE    CKF-REC
           ADD      1              TO   OUT-CNT-SYKCKF
     END-IF.
*2014.09.05↑
*
 210-WRITE-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    出荷梱包（取引明細）READ                     *
*--------------------------------------------------------------*
 900-MEF-READ           SECTION.
*
     MOVE     "MEF-READ  "        TO   S-NAME.
*
     IF       MEF-END   =   1
              GO        TO        900-MEF-READ-EXIT
     END-IF.
*
     READ     SYKMEF
         AT END
*****         MOVE      HIGH-VALUE     TO   MEF-KEY
              MOVE      ALL "9"        TO   MEF-KEY
              MOVE      1              TO   MEF-END
         NOT AT END
              MOVE      MEF-F05        TO   MEF-NOUHIN
              MOVE      MEF-F02        TO   MEF-DENNO
              MOVE      MEF-F03        TO   MEF-GYO
              ADD       1              TO   IN-CNT-MEF
     END-READ.
*
 900-MEF-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    出荷梱包（欠品情報）READ                     *
*--------------------------------------------------------------*
 900-KPF-READ           SECTION.
*
     MOVE     "KPF-READ  "        TO   S-NAME.
*
     IF       KPF-END   =   1
              GO        TO        900-KPF-READ-EXIT
     END-IF.
*
     READ     SYKKPF
         AT END
*****         MOVE      HIGH-VALUE   TO   KPF-KEY
              MOVE      ALL "9"      TO   KPF-KEY
              MOVE      1            TO   KPF-END
         NOT AT END
              ADD       1            TO   IN-CNT-KPF
     END-READ.
*
     IF     ( KPF-F08   =  KPF-NOUHIN )  AND
            ( KPF-F05   =  KPF-DENNO  )  AND
            ( KPF-F06   =  KPF-GYO    )
              DISPLAY
                NC"！！！！！！！！！！！！！！！"    UPON CONS
              DISPLAY
                NC"　欠品情報が重複しています　　"    UPON CONS
              DISPLAY
                NC"　　【欠品情報" IN-CNT-KPF NC"件目】　"
                                                     UPON CONS
              DISPLAY
                NC"　　【納品日　＝" KPF-F08 "  "     NC"】　"
                                                      UPON CONS
              DISPLAY
                NC"　　【伝票番号＝" KPF-F05          NC"】　"
                                                      UPON CONS
              DISPLAY
                NC"　　【行番号　＝" KPF-F06 "      " NC"】　"
                                                      UPON CONS
              DISPLAY
                NC"　処理を中止します　　　　　　"    UPON CONS
              DISPLAY
                NC"！！！！！！！！！！！！！！！"    UPON CONS
              MOVE    4000         TO   PROGRAM-STATUS
              STOP    RUN
     ELSE
          IF  KPF-END   NOT = 1
              MOVE      KPF-F08        TO   KPF-NOUHIN
              MOVE      KPF-F05        TO   KPF-DENNO
              MOVE      KPF-F06        TO   KPF-GYO
          END-IF
     END-IF.
*
 900-KPF-READ-EXIT.
     EXIT.
*
*-----------------<< PROGRAM END >>----------------------------*
