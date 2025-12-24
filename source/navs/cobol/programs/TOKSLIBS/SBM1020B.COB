****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　イオン流通ＢＭＳオンライン　　　　*
*    モジュール名　　　　：　請求データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　2012/11/13                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求合計ファイルより　　　　　　　*
*　　　　　　　　　　　　：　送信用の請求メッセージを作成する  *
*　　　　　　　　　　　　：　イオン分　　　　　　　　　　　　  *
*    作成日／更新日　　　：　2013/02/28                        *
*    作成者／更新者　　　：　ＮＡＶ井上　　　　　　　　　　　　*
*    更新概要　　　　　　：　サンデー・ジョイ追加　　　　　　　*
*　　　　　　　　　　　　：　　送信者ＩＤ転送部の条件を追加　  *
*    作成日／更新日　　　：　2015/06/04                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    更新概要　　　　　　：　店舗ＣＤ４桁⇒５桁対応　　　　　　*
*    作成日／更新日　　　：　2019/09/30                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    更新概要　　　　　　：　消費税軽減税率対応　　　　　　　　*
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBM1020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/11/13.
 DATE-COMPILED.
 SECURITY.              NONE.
*
 ENVIRONMENT            DIVISION.
*
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 請求合計ファイル>>-*
     SELECT   SETGKF    ASSIGN         DA-01-VS-SETGKFP
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEI-ST.
*----<< 発注メッセージ >>--*
     SELECT   BMSHACF   ASSIGN         DA-01-VI-BMSHACL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  HAC-F013  HAC-F308
                                       HAC-F346  HAC-F302
                                       HAC-F402
                        STATUS         HAC-ST.
*----<< 請求メッセージ >>--*
     SELECT  BMSSEKF    ASSIGN         DA-01-VI-BMSSEKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEK-F01
                        STATUS         SEK-ST.
*----<< 取引先振分マスタ >>--*
     SELECT   IONFURF   ASSIGN         DA-01-IONFURL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  IOF-F03  IOF-F02
                        STATUS         IOF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 請求合計ファイル>>-*
 FD  SETGKF             LABEL RECORD   IS   STANDARD.
***********             BLOCK  CONTAINS  15 RECORDS.
     COPY     SETGKFP   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 発注メッセージ >>--*
 FD  BMSHACF            LABEL RECORD   IS   STANDARD.
     COPY     BMSHACF   OF        XFDLIB
              JOINING   HAC       PREFIX.
*----<< 請求メッセージ >>--*
 FD  BMSSEKF            LABEL RECORD   IS   STANDARD.
     COPY     BMSSEKF   OF        XFDLIB
              JOINING   SEK       PREFIX.
*----<< 取引先振分マスタ  >>--*
 FD  IONFURF            LABEL RECORD   IS   STANDARD.
     COPY     IONFURF   OF        XFDLIB
              JOINING   IOF       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  INV-FLG        PIC  9(01)    VALUE  ZERO.
     03  END-FLG        PIC  9(01)    VALUE  ZERO.
     03  IDX            PIC  9(01)    VALUE  ZERO.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  OUT-CNT        PIC  9(07).
     03  SKIP-CNT       PIC  9(07).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HAC-ST             PIC  X(02).
 01  IOF-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  SEK-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
*
*----<< ｼｭｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-KEN        PIC  9(05)     VALUE  ZERO.
     03  KEI-KIN        PIC S9(09)     VALUE  ZERO.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-AREA.
     03  WK-BR-TOKCD    PIC  X(08).
     03  WK-BR-TENCD    PIC  X(05).
     03  WK-TEGAKI-SW   PIC  X(01).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
*
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
****************************************************************
 LINKAGE           SECTION.
****************************************************************
*01  LINK-INAREA.
 01  PARA-INDATE                 PIC  9(08).
 01  PARA-INTIME                 PIC  9(04).
 01  PARA-INTOKCD                PIC  9(08).
 01  PARA-INBMNCD                PIC  X(04).
 01  PARA-INTANCD                PIC  X(02).
 01  LINK-KENSU                  PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-INDATE PARA-INTIME
                                  PARA-INTOKCD PARA-INBMNCD
                                  PARA-INTANCD
                                  LINK-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 請求合計ファイル >>--*
 SETGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SETGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SBM1020B SETGKFP  ERROR " SEI-ST  " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 発注メッセージ  >>--*
 BMSHACF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      BMSHACF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SBM1020B BMSHACL2 ERROR " HAC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求メッセージ  >>--*
 BMSSEKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      BMSSEKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SBM1020B BMSSEKL1 ERROR " SEK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先振分マスタ  >>--*
 IONFURF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      IONFURF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SBM1020B IONFURL2 ERROR " IOF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SBM1020B HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 GENERAL-PROCESS        SECTION.
*
     PERFORM  INIT-RTN.
     PERFORM       MAIN-RTN
              UNTIL END-FLG = 1.
     PERFORM  END-RTN.
     STOP RUN.
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBM1020B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     SETGKF   BMSHACF.
     OPEN     INPUT     HTOKMS   IONFURF.
     OPEN     OUTPUT    BMSSEKF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
     INITIALIZE         WK-AREA.
***  TEST MSG
     DISPLAY  "PARA-DATE="  PARA-INDATE
             ",PARA-TIME="  PARA-INTIME
             ",PARA-TOKCD="  PARA-INTOKCD
             UPON  CONS.
***
*2013/02/28↓追加  請求データ作成対象取引先チェック
     IF       PARA-INTOKCD   =   00008737  OR
                                 00033189  OR
                                 00030402
                                 CONTINUE
     ELSE
              DISPLAY  NC"作成対象の取引先ではありません！"
                                 UPON   CONS
              MOVE     1         TO     END-FLG
              GO                 TO     INIT-RTN-EXIT
     END-IF.
*2013/02/28↑追加
*
     PERFORM  SEI-READ.
*
     IF       END-FLG  =  1
              DISPLAY  NC"＃　流通ＢＭＳ請求ＭＳＧ　作成対象無"
                       NC"　＃"    UPON  CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
     IF  SEI-F07  =  42
         ADD       1         TO   SKIP-CNT
         GO  TO    MAIN-100
     END-IF.
*
     IF  WK-BR-TOKCD   NOT  =   SEI-F01
         PERFORM   TOK-READ
*
         MOVE   SEI-F01      TO   WK-BR-TOKCD
     END-IF.
*
*----IF  WK-BR-TENCD  NOT  =   SEI-F03
*----    PERFORM  IONFURF-READ
*----    MOVE   SEI-F03      TO   WK-BR-TENCD
*----END-IF.
*
     PERFORM  IONFURF-READ.
     IF  INV-FLG =   1
         DISPLAY
                NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃"
                                                     UPON  CONS
         DISPLAY
                NC"＃　イオン取引先振分マスタに存在しない為　＃"
                                                     UPON  CONS
         DISPLAY
                NC"＃　処理を中止します。　　　　　　　　　　＃"
                                                     UPON  CONS
         DISPLAY
                NC"　　　　取引先コード＝"   SEI-F01 UPON  CONS
         DISPLAY
                NC"　　　　店　舗コード＝"   SEI-F03 UPON  CONS
         DISPLAY
                NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃"
                                                     UPON  CONS
         MOVE 1    TO    END-FLG
         MOVE 4000 TO    PROGRAM-STATUS
         GO        TO    MAIN-RTN-EXIT
     END-IF.
*店舗ＣＤ＝４桁で発注ワーク検索
     PERFORM  BMSHACF-READ.
*##2015/06/04 NAV ST  店舗ＣＤ＝４桁で検索し検索できなかった場合
     IF  WK-TEGAKI-SW  =  "Y"
         PERFORM  BMSHACF5-READ
     END-IF.
*##2015/06/04 NAV ED
     PERFORM  SEKSET-SEC.
     WRITE    SEK-REC.
     ADD      1   TO   OUT-CNT.
*
*
 MAIN-100.
     PERFORM  SEI-READ.
*
*
 MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　終了処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    SETGKF   BMSHACF.
     CLOSE    HTOKMS   IONFURF.
     CLOSE    BMSSEKF.
*
     DISPLAY "+++ ｾｲｷｭｳｺﾞｳｹｲ INPUT=" IN-CNT  UPON CONS.
     DISPLAY "+++ ｾｲｷｭｳｺﾞｳｹｲ SKIP =" SKIP-CNT  UPON  CONS.
     DISPLAY  NC"＃　流通ＢＭＳ請求ＭＳＧ作成件数　＝　"
              OUT-CNT  NC"　＃"    UPON  CONS.
*内部統制対応
     MOVE     OUT-CNT        TO      LINK-KENSU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBM1020B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS        UPON CONS.
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　請求合計ＦＲＥＡＤ　　　　　　　　　　　　　　　　　　*
****************************************************************
 SEI-READ               SECTION.
     READ     SETGKF    AT   END
              MOVE      1         TO   END-FLG
              GO   TO   SEI-READ-EXIT
     END-READ.
*
     ADD      1              TO   IN-CNT.
*
 SEI-READ-EXIT.
****************************************************************
*　　　　取引先マスタＲＥＡＤ　　　　　　　　　　　　　　　　　*
****************************************************************
 TOK-READ               SECTION.
     MOVE     SEI-F01       TO   TOK-F01.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F04
     END-READ.
 TOK-READ-EXIT.
     EXIT.
****************************************************************
*　　　　取引先振分マスタＲＥＡＤ　　　　　　　　　　　　　　　*
****************************************************************
 IONFURF-READ           SECTION.
     MOVE     SEI-F01       TO   IOF-F03.
     MOVE     SEI-F03       TO   IOF-F02.
     READ     IONFURF   INVALID
              MOVE      ZERO           TO   IOF-F01
              MOVE      1              TO   INV-FLG
*       DISPLAY  "IONFURF INV " "KEY="
*             SEI-F01  ","  SEI-F03  UPON  CONS
     END-READ.
 IONFURF-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　発注メッセージＲＥＡＤ　　　　　　　　　　　　　　　　*
****************************************************************
 BMSHACF-READ           SECTION.
     MOVE     SEI-F01       TO   HAC-F013.
*----MOVE     SEI-F03       TO   HAC-F308.
     MOVE     SEI-F03(2:4)  TO   HAC-F308.
*  納品日
     MOVE    "3"                 TO  LINK-IN-KBN.
     MOVE    SEI-F10             TO  LINK-IN-YMD6.
     CALL "SKYDTCKB"  USING  LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD8.
     IF  LINK-OUT-RET = ZERO
         MOVE LINK-OUT-YMD8      TO  SYS-DATEW
     ELSE
         MOVE ZERO               TO  SYS-DATEW
     END-IF.
     MOVE    SYS-DATEW      TO    HAC-F346.
     MOVE     SEI-F05(3:7)  TO   HAC-F302.
     MOVE     "0001"        TO   HAC-F402.
     MOVE     SPACE         TO   WK-TEGAKI-SW.
*****DISPLAY "HAC-F013 4 = " HAC-F013  UPON CONS.
*    DISPLAY "HAC-F308 4 = " HAC-F308  UPON CONS.
*    DISPLAY "HAC-F346 4 = " HAC-F346  UPON CONS.
*    DISPLAY "HAC-F302 4 = " HAC-F302  UPON CONS.
*****DISPLAY "HAC-F402 4 = " HAC-F402  UPON CONS.
     READ     BMSHACF   INVALID
              MOVE      "Y"            TO   WK-TEGAKI-SW
     END-READ.
*****DISPLAY "WK-TEGAKI4 = " WK-TEGAKI-SW UPON CONS.
 BMSHACF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　発注メッセージＲＥＡＤ　　　　　　　　　　　　　　　　*
****************************************************************
 BMSHACF5-READ          SECTION.
     MOVE     SEI-F01       TO   HAC-F013.
     MOVE     SEI-F03       TO   HAC-F308.
*  納品日
     MOVE    "3"                 TO  LINK-IN-KBN.
     MOVE    SEI-F10             TO  LINK-IN-YMD6.
     CALL "SKYDTCKB"  USING  LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD8.
     IF  LINK-OUT-RET = ZERO
         MOVE LINK-OUT-YMD8      TO  SYS-DATEW
     ELSE
         MOVE ZERO               TO  SYS-DATEW
     END-IF.
     MOVE    SYS-DATEW      TO    HAC-F346.
     MOVE     SEI-F05(3:7)  TO   HAC-F302.
     MOVE     "0001"        TO   HAC-F402.
     MOVE     SPACE         TO   WK-TEGAKI-SW.
*****DISPLAY "HAC-F013 5 = " HAC-F013  UPON CONS.
*    DISPLAY "HAC-F308 5 = " HAC-F308  UPON CONS.
*    DISPLAY "HAC-F346 5 = " HAC-F346  UPON CONS.
*    DISPLAY "HAC-F302 5 = " HAC-F302  UPON CONS.
*****DISPLAY "HAC-F402 5 = " HAC-F402  UPON CONS.
     READ     BMSHACF   INVALID
              MOVE      "Y"            TO   WK-TEGAKI-SW
     END-READ.
*****DISPLAY "WK-TEGAKI5 = " WK-TEGAKI-SW UPON CONS.
 BMSHACF5-READ-EXIT.
     EXIT.
****************************************************************
*　　　　請求メッセージ用転送　　　　　　　　　　　　　　　　　*
****************************************************************
 SEKSET-SEC             SECTION.
*
     MOVE    SPACE           TO   SEK-REC.
     INITIALIZE                   SEK-REC.
*＜共通キー＞-----------------------------------------
*　処理日
     MOVE    PARA-INDATE    TO    SEK-F011.
*　処理時刻
     MOVE    PARA-INTIME    TO    SEK-F012.
*　取引先コード
     MOVE    PARA-INTOKCD   TO    SEK-F013.
*　送信者部門コード
     MOVE    PARA-INBMNCD   TO    SEK-F98.
*　送信者担当者コード
     MOVE    PARA-INTANCD   TO    SEK-F99.
*
*＜メッセージヘッダ＞---------------------------------
*　レコード区分
     MOVE    "A"            TO    SEK-F101.
*　ヘッダバージョン
     MOVE    "1.3"          TO    SEK-F102.
*　送信者ＩＤ
*    2013/02/28↓変更
*****IF      PARA-INTOKCD   =     00008737
*****        MOVE    "4974449000005"   TO    SEK-F103
*****ELSE
*****        MOVE    "4974449000012"   TO    SEK-F103
*****END-IF.
     EVALUATE   PARA-INTOKCD
*               ↓イオン
       WHEN     00008737
                MOVE "4974449000005"   TO    SEK-F103
*               ↓イオン九州
       WHEN     00033189
                MOVE "4974449000012"   TO    SEK-F103
*               ↓サンデー・ジョイ
       WHEN     00030402
                MOVE "4974449000029"   TO    SEK-F103
*               ↓上記以外　　　　
       WHEN     OTHER
                MOVE "?????????????"   TO    SEK-F103
     END-EVALUATE.
*    2013/02/28↑変更
*　送信者ＩＤ発行元
     MOVE    "GLN"          TO    SEK-F104.
*　受信者ＩＤ イオン、サンデー・ジョイ共通
     MOVE    "4901810000005"   TO    SEK-F105.
*　受信者ＩＤ発行元
     MOVE    "GLN"          TO    SEK-F106.
*　流通ＢＭＳ名称
     MOVE    "SECONDGENEDI"    TO    SEK-F107.
*　バージョン
     MOVE    "1P"           TO    SEK-F108.
*　メッセージ種
     MOVE    "INVOICE"      TO    SEK-F110.
*　作成日時
     MOVE    "3"                 TO  LINK-IN-KBN.
     MOVE    SYS-DATE            TO  LINK-IN-YMD6.
     CALL "SKYDTCKB"  USING  LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD8.
     IF  LINK-OUT-RET = ZERO
         MOVE LINK-OUT-YMD8      TO  SYS-DATEW
     ELSE
         MOVE ZERO               TO  SYS-DATEW
     END-IF.
     MOVE    SYS-DATEW      TO    SEK-F112(1:8).
     MOVE    SYS-TIME(1:6)  TO    SEK-F112(9:6).
*　インスタンスＩＤ
*----MOVE    "?X80"         TO    SEK-F109.
     MOVE    SYS-DATEW(3:6) TO    SEK-F109(1:6).
     MOVE    SYS-TIME(1:4)  TO    SEK-F109(7:4).
*　テスト区分（テストモードの場合、”１”をセット）
     MOVE    0              TO    SEK-F115.
*****MOVE    1              TO    SEK-F115.
*　メッセージ識別ＩＤ
*----MOVE    "F109?"        TO    SEK-F119.
     MOVE    "MSG-"         TO    SEK-F119(1:4).
     MOVE    SEK-F109(1:10) TO    SEK-F119(5:10).
*
*＜請求リスト＞---------------------------------------
*　レコード区分
     MOVE    "B"            TO    SEK-F201.
*　ＸＭＬ内容バージョン
     MOVE    "1.3"          TO    SEK-F202.
*　ＸＭＬ構造バージョン
     MOVE    "1.3"          TO    SEK-F203.
*　支払法人コード
     MOVE    IOF-F01        TO    SEK-F206.
*　支払法人コードＧＬＮ
     MOVE    0              TO    SEK-F207.
*　発注者コード
     MOVE    IOF-F01        TO    SEK-F210.
*　発注者ＧＬＮ
     MOVE    0              TO    SEK-F211.
*
*＜請求＞----------------------------------------------
*　レコード区分
     MOVE    "C"            TO    SEK-F301.
*　請求取引先コード
*----MOVE    PARA-INTOKCD         TO    SEK-F303.
     MOVE    PARA-INTOKCD(4:5)    TO    SEK-F303.
*　請求取引先ＧＬＮ
     MOVE    0              TO    SEK-F304.
*　請求取引先名称カナ
     MOVE    TOK-F04        TO    SEK-F306.
*  対象期間終了
     MOVE    "3"                 TO  LINK-IN-KBN.
     MOVE    SEI-F02             TO  LINK-IN-YMD6.
     CALL "SKYDTCKB"  USING  LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD8.
     IF  LINK-OUT-RET = ZERO
         MOVE LINK-OUT-YMD8      TO  SYS-DATEW
     ELSE
         MOVE ZERO               TO  SYS-DATEW
     END-IF.
     MOVE    SYS-DATEW      TO    SEK-F308.
*
*＜取引＞---------------------------------------------
*　レコード区分
     MOVE    "D"            TO    SEK-F401.
*  伝票番号
*----MOVE    SEI-F05(1:8)   TO    SEK-F402.
*----MOVE    SEI-F05(1:6)   TO    SEK-F402.
     MOVE    SEI-F05(3:6)   TO    SEK-F402.
*  計上部署コード
*----MOVE    SEI-F03        TO    SEK-F405.
*##2015/06/04 NAV ST
*****MOVE    SEI-F03(2:4)   TO    SEK-F405.
     MOVE    SEI-F03        TO    SEK-F405.
*##2015/06/04 NAV ED
*  計上部署ＧＬＮ
     MOVE    0              TO    SEK-F406.
*  手書き判定
*    オンライン
     IF  WK-TEGAKI-SW  =  SPACE
*        商品分類（大）
         MOVE    HAC-F342       TO    SEK-F413
*        商品分類（中）
         MOVE    HAC-F343       TO    SEK-F414
*    手書き
     ELSE
*        商品分類（大）
         MOVE    "000"          TO    SEK-F413
*        商品分類（中）
         MOVE    SEI-F12        TO    SEK-F414
     END-IF.
*  計上日
     MOVE    "3"                 TO  LINK-IN-KBN.
     MOVE    SEI-F10             TO  LINK-IN-YMD6.
     CALL "SKYDTCKB"  USING  LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD8.
     IF  LINK-OUT-RET = ZERO
         MOVE LINK-OUT-YMD8      TO  SYS-DATEW
     ELSE
         MOVE ZERO               TO  SYS-DATEW
     END-IF.
     MOVE    SYS-DATEW      TO    SEK-F415.
*　請求金額
     MOVE    SEI-F06        TO    SEK-F416.
*　伝区判定
     IF  SEI-F07  =  40
*　　　　請求金額符号
         MOVE    "+"            TO    SEK-F417
*　　　　支払内容
         MOVE    "1001"         TO    SEK-F421
     END-IF.
     IF  SEI-F07 =   41
*　　　　請求金額符号
         MOVE    "-"            TO    SEK-F417
*　　　　支払内容
         MOVE    "1002"         TO    SEK-F421
     END-IF.
*税区分
*****MOVE        "02"           TO    SEK-F422.
     MOVE        "05"           TO    SEK-F422.
*#2019/09/30 NAV ST 消費税軽減税率対応　消費税率をセット
*****IF  SEK-F415  >=  20191001
         MOVE    SEI-F16            TO    SEK-F423.
*****END-IF.
*#2019/09/30 NAV ED
*
 SEKSET-EXIT.
     EXIT.
*
