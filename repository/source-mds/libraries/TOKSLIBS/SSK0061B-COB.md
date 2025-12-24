# SSK0061B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSK0061B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス　　　　　        *
*    モジュール名　　　　：　返品伝票計上処理　　　　　　　　　*
*    作成日／更新日　　　：　2014/03/26                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタにて　　　　　　　　　　  *
*    　　　　　　　　　　　　　・担当者ＣＤ範囲　　　　　　　  *
*                            　・入力日範囲　を受け取り、　　　*
*                            合致する受領返品累積データから　　*
*                            　・売上伝票Ｆ作成　　　　        *
*                            　・在庫マスタ更新（返品のみ）    *
*                            　・受信状況ファイル更新　を行う。*
*    作成日／更新日　　　：　2014/07/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　請求区分セット変更　　　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSK0061B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2014/03/26.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領返品累積ファイル
     SELECT   KEIJHRF   ASSIGN    TO        DA-01-VI-KEIJHRL6
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JHR-F80  JHR-F82
                                            JHR-F83  JHR-F02
                                            JHR-F07  JHR-F04
                                            JHR-F05
                        FILE  STATUS   IS   JHR-STATUS.
*売上伝票ファイル
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01  DEN-F02
                                            DEN-F04  DEN-F051
                                            DEN-F07  DEN-F112
                                            DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*在庫マスタ
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01  ZAI-F021
                                            ZAI-F022 ZAI-F031
                                            ZAI-F032 ZAI-F033
                        FILE  STATUS   IS   ZAI-STATUS.
*商品変換テーブル（キー１：取ＣＤ、相手商品ＣＤ）
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TB1-F01
                                            TB1-F02
                        FILE  STATUS   IS   TB1-STATUS.
*商品変換テーブル
     SELECT   SHOTBL2   ASSIGN    TO        DA-01-VI-SHOTBL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TB2-F01   TB2-F04
                                            TB2-F031  TB2-F0321
                                            TB2-F0322 TB2-F0323
                        FILE  STATUS   IS   TB2-STATUS.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-STATUS.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-STATUS.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-STATUS.
*受領受信状況ファイル
     SELECT   KEIJYOF   ASSIGN    TO        DA-01-VI-KEIJYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KJY-F01  KJY-F02
                                            KJY-F03  KJY-F04
                        FILE      STATUS    KJY-STATUS.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01  JYO-F02
                        FILE      STATUS    JYO-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領返品累積ファイル
******************************************************************
 FD  KEIJHRF            LABEL RECORD   IS   STANDARD.
     COPY     KEIJHRF   OF        XFDLIB
              JOINING   JHR       PREFIX.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENF            LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
******************************************************************
*    在庫マスタ
******************************************************************
 FD  ZAMZAIF            LABEL RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
******************************************************************
*    商品変換テーブル（キー１）
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TB1       PREFIX.
******************************************************************
*    商品変換テーブル（キー２）
******************************************************************
 FD  SHOTBL2            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TB2       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    受領受信状況ファイル
******************************************************************
 FD  KEIJYOF            LABEL RECORD   IS   STANDARD.
     COPY     KEIJYOF   OF        XFDLIB
              JOINING   KJY       PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  IN-CNT                  PIC  9(07)     VALUE  ZERO.
 01  DEN-ADD-CNT             PIC  9(07)     VALUE  ZERO.
 01  TAISYO-CNT              PIC  9(07)     VALUE  ZERO.
 01  SHTDENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHOTBL2-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  KEIJYOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-TANABAN              PIC  X(06)     VALUE  SPACE.
 01  WK-JHR-F16              PIC  9(05)V9(01) VALUE  ZERO.
*システム日付の編集
 01  WK-DTTM-AREA.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
     03  SYS-TIME.
         05  WK-SYS-TIME1  PIC 9(06).
         05  WK-SYS-TIME2  PIC 9(02).
*ファイルＳＴＡＴＵＳエリア
 01  WK-ST.
     03  JHR-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  ZAI-STATUS        PIC  X(02).
     03  TB1-STATUS        PIC  X(02).
     03  TB2-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  KJY-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*メッセージ定義エリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0061B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0061B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0061B".
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
*在庫締め日退避ワーク
 01  WK-ZAIKO-SIME          PIC   9(06)  VALUE  ZERO.
 01  WK-SIMEBI.
     03  WK-SIMEBI-YY       PIC   9(04).
     03  WK-SIMEBI-MM       PIC   9(02).
*数量変換ワーク
 01  WK-SURYO.
     03  WK-SURYO-HEN       PIC   9(05)V9(01)  VALUE  ZERO.
*伝票備考ワーク
 01  WK-BIKO.
     03  WK-BIKO-1          PIC   X(15)  VALUE  SPACE.
     03  WK-BIKO-2          PIC   X(15)  VALUE  SPACE.
*ブレイクキー
 01  WK-KEY.
     03  WK-JHR-F02         PIC   9(05)  VALUE  ZERO.
     03  WK-JHR-F07         PIC   9(08)  VALUE  ZERO.
     03  WK-JHR-F04         PIC   9(09)  VALUE  ZERO.
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*売上伝票ファイル内容格納
     COPY   SHTDENF  OF XFDLIB  JOINING   DWK  AS   PREFIX.
*
 LINKAGE                SECTION.
 01  LINK-BMNCD              PIC  X(04).
 01  LINK-TANCD              PIC  X(02).
 01  LINK-SKBN               PIC  X(01).
 01  LINK-DKBN               PIC  X(01).
 01  LINK-DFROM              PIC  9(08).
 01  LINK-DTO                PIC  9(08).
 01  LINK-KKBN               PIC  X(01).
 01  LINK-TANFROM            PIC  X(02).
 01  LINK-TANTO              PIC  X(02).
 01  LINK-DENK1              PIC  X(02).
 01  LINK-DENK2              PIC  X(02).
 01  LINK-DENK3              PIC  X(02).
 01  LINK-DENK4              PIC  X(02).
 01  LINK-DENK5              PIC  X(02).
 01  LINK-TENFROM            PIC  9(05).
 01  LINK-TENTO              PIC  9(05).
 01  LINK-DENNOFROM          PIC  9(09).
 01  LINK-DENNOTO            PIC  9(09).
 01  LINK-SBSFROM            PIC  X(02).
 01  LINK-SBSTO              PIC  X(02).
 01  LINK-DENKFROM           PIC  X(02).
 01  LINK-DENKTO             PIC  X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION   USING  LINK-BMNCD
                                         LINK-TANCD
                                         LINK-SKBN
                                         LINK-DKBN
                                         LINK-DFROM
                                         LINK-DTO
                                         LINK-KKBN
                                         LINK-TANFROM
                                         LINK-TANTO
                                         LINK-DENK1
                                         LINK-DENK2
                                         LINK-DENK3
                                         LINK-DENK4
                                         LINK-DENK5
                                         LINK-TENFROM
                                         LINK-TENTO
                                         LINK-DENNOFROM
                                         LINK-DENNOTO
                                         LINK-SBSFROM
                                         LINK-SBSTO
                                         LINK-DENKFROM
                                         LINK-DENKTO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJHRF.
     MOVE      "KEIJHRL4"   TO   AB-FILE.
     MOVE      JHR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAMZAIF.
     MOVE      "ZAMZAIL1"   TO   AB-FILE.
     MOVE      ZAI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TB1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL2.
     MOVE      "SHOTBL2 "   TO   AB-FILE.
     MOVE      TB2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS1  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KEIJYOF.
     MOVE      "KEIJYOL1"   TO   AB-FILE.
     MOVE      KJY-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
     STOP  RUN.
*
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     HSHOTBL  SHOTBL2  HMEIMS  HTOKMS
                        HTENMS   HJYOKEN.
     OPEN     I-O       KEIJHRF  SHTDENF  ZAMZAIF  KEIJYOF.
*パラメタをメッセージをして表示
     DISPLAY "## LINK-TANFROM = " LINK-TANFROM UPON CONS.
     DISPLAY "## LINK-TANTO   = " LINK-TANTO   UPON CONS.
     DISPLAY "## LINK-DFROM   = " LINK-DFROM   UPON CONS.
     DISPLAY "## LINK-DTO     = " LINK-DTO     UPON CONS.
*
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT      SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT      SYS-DATE  FROM      DATE.
*システム日付８桁変換
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
*条件ファイル索引（在庫締め日取得）
     MOVE        99             TO   JYO-F01.
     MOVE       "ZAI"           TO   JYO-F02.
     PERFORM  HJYOKEN-READ-SEC.
     IF   HJYOKEN-INV-FLG  =  "INV"
          DISPLAY NC"＃＃条件Ｆ取得エラー（在庫締）＃＃"
                  UPON CONS
          MOVE   4000           TO   PROGRAM-STATUS
          STOP  RUN
     END-IF.
*在庫締め年月を算出する
     MOVE        JYO-F05        TO   WK-ZAIKO-SIME.
     MOVE        WK-ZAIKO-SIME  TO   WK-SIMEBI.
     ADD         1              TO   WK-SIMEBI-MM.
     IF  WK-SIMEBI-MM > 12
         ADD     1              TO   WK-SIMEBI-YY
         MOVE    1              TO   WK-SIMEBI-MM
     END-IF.
     DISPLAY NC"＃＃在庫締日　＝　" WK-SIMEBI NC"　＃＃"
             UPON CONS.
*受領返品累積ファイルスタート
     PERFORM  KEIJHRF-START-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*　　受領返品累積ファイルスタート
****************************************************************
 KEIJHRF-START-SEC     SECTION.
*
     MOVE    "KEIJHRF-START-SEC" TO   S-NAME.
*スタートの為、レコード初期化
     MOVE     SPACE              TO   JHR-REC.
     INITIALIZE                       JHR-REC.
*スタートキーセット
     MOVE     "1"                TO   JHR-F80.
     MOVE     LINK-DFROM         TO   JHR-F83.
     MOVE     LINK-TANFROM       TO   JHR-F82.
*
     START  KEIJHRF  KEY  IS  >=  JHR-F80  JHR-F82  JHR-F83
                                  JHR-F02  JHR-F07  JHR-F04
                                  JHR-F05
           INVALID
           MOVE  "END"           TO      END-FLG
           DISPLAY "＃処理対象がありません１＃" UPON CONS
           GO                    TO      KEIJHRF-START-EXIT
     END-START.
*取込数量確定ファイル読込
     PERFORM  KEIJHRF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY "＃処理対象がありません２＃" UPON CONS
     ELSE  *>ブレイクキーセット
           MOVE  JHR-F02         TO      WK-JHR-F02
           MOVE  JHR-F07         TO      WK-JHR-F07
           MOVE  JHR-F04         TO      WK-JHR-F04
           MOVE  SPACE           TO      WK-BIKO-1
           MOVE  SPACE           TO      WK-BIKO-2
     END-IF.
*
 KEIJHRF-START-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*キーブレイク確認
     IF  JHR-F02  NOT =  WK-JHR-F02
     OR  JHR-F07  NOT =  WK-JHR-F07
     OR  JHR-F04  NOT =  WK-JHR-F04
         IF  WK-BIKO-1  =  SPACE
         AND WK-BIKO-2  =  SPACE
             CONTINUE
         ELSE  *>備考行の出力
             PERFORM  DEN-BIKO-SEC
         END-IF
         MOVE SPACE              TO   WK-BIKO-1  WK-BIKO-2
         MOVE JHR-F02            TO   WK-JHR-F02
         MOVE JHR-F07            TO   WK-JHR-F07
         MOVE JHR-F04            TO   WK-JHR-F04
         MOVE SPACE              TO   DWK-REC
         INITIALIZE                   DWK-REC
     END-IF.
*レコード初期化
     MOVE     SPACE              TO   DEN-REC.
     INITIALIZE                       DEN-REC.
*マスタを索引する。
*    商品変換ＴＢＬ索引
     PERFORM  HSHOTBL-READ-SEC.
*    存在しない場合、又は、ＪＡＮＣＤが空白の場合
     IF  HSHOTBL-INV-FLG = "INV"
     OR  JHR-F09  =  SPACE
         PERFORM  SHOTBL2-READ-SEC
*        存在しない場合、_番を空白に設定
         IF  SHOTBL2-INV-FLG  =  "INV"
             MOVE SPACE          TO  WK-TANABAN
         ELSE   *>存在した場合
             MOVE TB2-F08        TO  WK-TANABAN
         END-IF
     ELSE       *>相手商品ＣＤで存在した場合
         MOVE     TB1-F08        TO  WK-TANABAN
     END-IF.
*取引先マスタ索引
     PERFORM HTOKMS-READ-SEC.
*店舗マスタ索引
     PERFORM HTENMS-READ-SEC.
*商品名称マスタ索引
     PERFORM HMEIMS-READ-SEC.
*項目セット
     MOVE    JHR-F01            TO  DEN-F01.
     MOVE    JHR-F04            TO  DEN-F02.
     MOVE    JHR-F05            TO  DEN-F03.
     MOVE    ZERO               TO  DEN-F04.
     MOVE    JHR-F23            TO  DEN-F051.
     EVALUATE DEN-F051
         WHEN "41"  MOVE NC"売上返品" TO DEN-F052
         WHEN "42"  MOVE NC"売上値引" TO DEN-F052
     END-EVALUATE.
     MOVE    LINK-TANCD         TO  DEN-F06.
     MOVE    JHR-F02            TO  DEN-F07.
     MOVE    JHR-F08            TO  DEN-F08  DEN-F09.
     MOVE    JHR-F07            TO  DEN-F112.
     MOVE    JHR-F07            TO  DEN-F111.
     MOVE    JHR-F07            TO  DEN-F113.
*2014/07/08↓
     MOVE    JHR-F22            TO  DEN-F133.
*2014/07/08↑
*2014.04.15↓
     MOVE    "9"                TO  DEN-F134.
*2014.04.15↑
     MOVE    JHR-F10            TO  DEN-F1411.
     MOVE    JHR-F11            TO  DEN-F1412(1:5).
     MOVE    JHR-F12            TO  DEN-F1412(6:2).
     MOVE    JHR-F13            TO  DEN-F1412(8:1).
     MOVE    JHR-F14            TO  DEN-F1421.
     MOVE    JHR-F15            TO  DEN-F1422.
     MOVE    JHR-F16            TO  WK-SURYO-HEN.
     MOVE    WK-SURYO-HEN       TO  DEN-F15   DEN-F50.
     MOVE    "1"                TO  DEN-F16.
     MOVE    JHR-F17            TO  DEN-F172  DEN-F512.
     MOVE    JHR-F19            TO  DEN-F173  DEN-F513.
     COMPUTE DEN-F181 = DEN-F15 * DEN-F172.
     COMPUTE DEN-F182 = DEN-F15 * DEN-F173.
     MOVE    DEN-F181           TO  DEN-F521.
     MOVE    DEN-F182           TO  DEN-F522.
     MOVE    JHR-F21            TO  DEN-F22.
     MOVE    JHR-F04            TO  DEN-F23.
     MOVE    TOK-F52            TO  DEN-F24.
     MOVE    JHR-F09            TO  DEN-F25.
     MOVE    9                  TO  DEN-F272.
     MOVE    0                  TO  DEN-F274.
     MOVE    1                  TO  DEN-F275.
     MOVE    9                  TO  DEN-F276.
     MOVE    "A"                TO  DEN-F278.
     MOVE    9                  TO  DEN-F27B.
     IF  HTENMS-INV-FLG  =  SPACE
         MOVE    TEN-F04        TO  DEN-F30
     END-IF.
     MOVE    SYS-DATEW          TO  DEN-F99.
     MOVE    WK-TANABAN         TO  DEN-F49.
     MOVE    LINK-TANCD         TO  DEN-F59.
     MOVE    LINK-TANCD         TO  DEN-F60.
     MOVE    LINK-TANCD         TO  DEN-F61.
     MOVE    SYS-DATEW          TO  DEN-F62.
     MOVE    SYS-DATEW          TO  DEN-F63.
     MOVE    SYS-DATEW          TO  DEN-F64.
     MOVE    "1"                TO  DEN-F65.
     MOVE    "1"                TO  DEN-F66.
     MOVE    WK-SYS-TIME1       TO  DEN-F67.
*伝票備考１、２をワークに退避する
     IF  JHR-F24  NOT =  SPACE
         MOVE  JHR-F24          TO  WK-BIKO-1
     ELSE
         MOVE  SPACE            TO  WK-BIKO-1
     END-IF.
     IF  JHR-F25  NOT =  SPACE
         MOVE  JHR-F25          TO  WK-BIKO-2
     ELSE
         MOVE  SPACE            TO  WK-BIKO-2
     END-IF.
*備考行を作成の為、売上伝票ファイルの内容をワークに退避
     MOVE    DEN-REC            TO  DWK-REC.
*売上伝票ファイルの存在チェックを行なう
     PERFORM  SHTDENF-READ-SEC.
     IF  SHTDENF-INV-FLG = SPACE
         MOVE "1"               TO  JHR-F91  *>エラー区分セット
         REWRITE  JHR-REC
         GO                     TO  MAIN-020
     END-IF.
*在庫マスタ更新（伝区＝４１の時のみ在庫更新する）
     IF  DEN-F051  =   41
         PERFORM  ZAIKO-UPDT-SEC
     END-IF.
*売上伝票ファイル追加
     WRITE   DEN-REC.
     ADD     1                  TO  DEN-ADD-CNT.
*返品累積データ　担当者／部門／日付　計上確定セット
     MOVE    LINK-BMNCD         TO  JHR-F84.
     MOVE    LINK-TANCD         TO  JHR-F85.
     MOVE    SYS-DATEW          TO  JHR-F86.
     MOVE    SYS-DATEW          TO  JHR-F96.
     MOVE    WK-SYS-TIME1       TO  JHR-F97.
     MOVE    LINK-BMNCD         TO  JHR-F98.
     MOVE    LINK-TANCD         TO  JHR-F99.
*受領受信状況ファイル更新
     PERFORM  KEIJYOF-READ-SEC.
     IF  KEIJYOF-INV-FLG  =  SPACE
         ADD  1                 TO  KJY-F08
         REWRITE KJY-REC
     END-IF.
*
 MAIN-010.
     REWRITE JHR-REC.    *>返品累積データ更新
*
 MAIN-020.
     PERFORM  KEIJHRF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　伝票備考行出力処理　　　　                                *
****************************************************************
 DEN-BIKO-SEC                SECTION.
*
     MOVE    "DEN-BIKO-SEC"     TO        S-NAME.
*レコード初期化
     MOVE     SPACE             TO        DEN-REC.
     INITIALIZE                           DEN-REC.
*
     MOVE     DWK-REC           TO        DEN-REC.
*
     MOVE     80                TO        DEN-F03.
     MOVE     99                TO        DEN-F1411.
     MOVE     SPACE             TO        DEN-F1412.
     MOVE     WK-BIKO-1         TO        DEN-F1421.
     MOVE     WK-BIKO-2         TO        DEN-F1422.
     MOVE     ZERO              TO        DEN-F15  DEN-F171
                                          DEN-F172 DEN-F173
                                          DEN-F181 DEN-F182
                                          DEN-F19  DEN-F20.
*売上伝票ファイルの存在チェックを行なう
     PERFORM  SHTDENF-READ-SEC.
     IF  SHTDENF-INV-FLG = SPACE
         GO                     TO        DEN-BIKO-EXIT
     END-IF.
*売上伝票ファイル追加
     WRITE   DEN-REC.
     ADD     1                  TO        DEN-ADD-CNT.
*
 DEN-BIKO-EXIT.
     EXIT.
*
****************************************************************
*　　受領返品累積ファイル読込                                  *
****************************************************************
 KEIJHRF-READ-SEC            SECTION.
*
     MOVE    "KEIJHRF-READ-SEC" TO        S-NAME.
*
     READ  KEIJHRF  AT  END
           MOVE  "END"          TO        END-FLG
           GO                   TO        KEIJHRF-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                 TO        IN-CNT.
*処理経過出力
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "## READ-CNT = " IN-CNT " ##" UPON CONS
     END-IF.
 READ010.
*計上区分が１より大きい場合は終了へ
     IF  JHR-F80  >  "1"
         MOVE     "END"         TO        END-FLG
         GO                     TO        KEIJHRF-READ-EXIT
     END-IF.
 READ020.
*入力日範囲チェック（入力日数量以上になったら終了へ）
     IF  LINK-DTO    <  JHR-F83
         MOVE     "END"         TO        END-FLG
         GO                     TO        KEIJHRF-READ-EXIT
     END-IF.
 READ030.
*担当者範囲チェック
     IF  LINK-TANFROM  <=  JHR-F82
     AND LINK-TANTO    >=  JHR-F82
         CONTINUE
     ELSE
         GO                     TO        KEIJHRF-READ-SEC
     END-IF.
 READ040.
*計上日チェック（計上済のデータは対象外）
     IF  JHR-F86  >  ZERO
         GO                     TO        KEIJHRF-READ-SEC
     END-IF.
*
     ADD 1                      TO        TAISYO-CNT.
*
 KEIJHRF-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込（キー１）                *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      JHR-F01     TO     TB1-F01.
     MOVE      JHR-F09     TO     TB1-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込（キー２）                *
****************************************************************
 SHOTBL2-READ-SEC          SECTION.
*
     MOVE      "SHOTBL2-READ-SEC" TO    S-NAME.
*
     MOVE      JHR-F01     TO     TB2-F01.
     MOVE      JHR-F08     TO     TB2-F04.
     MOVE      JHR-F10     TO     TB2-F031.
     MOVE      JHR-F11     TO     TB2-F0321.
     MOVE      JHR-F12     TO     TB2-F0322.
     MOVE      JHR-F13     TO     TB2-F0323.
     READ      SHOTBL2
               INVALID
               MOVE      "INV"    TO    SHOTBL2-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SHOTBL2-INV-FLG
     END-READ.
*
 SHOTBL2-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
     MOVE      SPACE       TO     HMEIMS-INV-FLG.
*
     MOVE      JHR-F10     TO     MEI-F011.
     MOVE      JHR-F11     TO     MEI-F012(1:5).
     MOVE      JHR-F12     TO     MEI-F012(6:2).
     MOVE      JHR-F13     TO     MEI-F012(8:1).
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*                取引先マスタ　　　　　                        *
****************************************************************
 HTOKMS-READ-SEC           SECTION.
*
     MOVE      "HTOKMS-READ-SEC"  TO    S-NAME.
*
     MOVE      JHR-F01     TO     TOK-F01.
     READ      HTOKMS
               INVALID
*              取引先Ｍが未存在の場合、異常終了させる
               MOVE      "INV"    TO    HTOKMS-INV-FLG
               DISPLAY NC"＃＃取引先Ｍ未存在" " = " JHR-F01
                       " " NC"＃＃"  UPON  CONS
               MOVE       4000    TO    PROGRAM-STATUS
               STOP  RUN
               NOT  INVALID
               MOVE      SPACE    TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*                店舗マスタ読込　　　　                        *
****************************************************************
 HTENMS-READ-SEC           SECTION.
*
     MOVE      "HTENMS-READ-SEC"  TO    S-NAME.
*
     MOVE      173         TO     TEN-F52.
     MOVE      JHR-F02     TO     TEN-F011.
     READ      HTENMS
               INVALID
               MOVE      "INV"    TO    HTENMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*                ナフコ商品マスタ読込　                        *
****************************************************************
 KEIJYOF-READ-SEC          SECTION.
*
     MOVE      "KEIJYOF-READ-SEC" TO    S-NAME.
*
     MOVE      JHR-F78     TO     KJY-F01.
     MOVE      JHR-F79     TO     KJY-F02.
     MOVE      JHR-F01     TO     KJY-F03.
     MOVE      JHR-F06     TO     KJY-F04.
     READ      KEIJYOF
               INVALID
               MOVE      "INV"    TO    KEIJYOF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    KEIJYOF-INV-FLG
     END-READ.
*
 KEIJYOF-READ-EXIT.
     EXIT.
****************************************************************
*                条件ファイル索引　　　                        *
****************************************************************
 HJYOKEN-READ-SEC          SECTION.
*
     MOVE      "HJYOKEN-READ-SEC" TO    S-NAME.
*
     READ      HJYOKEN
               INVALID
               MOVE      "INV"    TO    HJYOKEN-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*                売上伝票ファイル索引　                        *
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     MOVE     "SHTDENF-READ-SEC"  TO    S-NAME.
*
     READ      SHTDENF
               INVALID
               MOVE      "INV"    TO    SHTDENF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*伝票備考行出力
     IF  WK-BIKO-1  =  SPACE
     AND WK-BIKO-2  =  SPACE
         CONTINUE
     ELSE  *>備考行の出力
         PERFORM  DEN-BIKO-SEC
     END-IF.
*件数印字
*読込カウント
     DISPLAY "## READ-CNT        = " IN-CNT           UPON CONS.
*対象カウント
     DISPLAY "## TAISYO-CNT      = " TAISYO-CNT       UPON CONS.
*売上作成カウント
     DISPLAY "## SHTDENF-ADD-CNT = " DEN-ADD-CNT      UPON CONS.
*
     CLOSE  HSHOTBL  SHOTBL2  HMEIMS  HTOKMS
            HTENMS   HJYOKEN
            KEIJHRF  SHTDENF  ZAMZAIF  KEIJYOF.
*
 END-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDT-SEC         SECTION.
*
     MOVE     "ZAIKO-UPDT-SEC"    TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     MOVE    DEN-F49         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*数量を変換
      MOVE      JHR-F16       TO   WK-JHR-F16.
*商品在庫マスタ項目セット
      MOVE      DEN-F08       TO   ZAI-F01.
      MOVE      DEN-F1411     TO   ZAI-F021.
      MOVE      DEN-F1412     TO   ZAI-F022.
      MOVE      DEN-F49       TO   ZAI-F03.
*当月分／次月分の判定
*     次月の場合
     IF  WK-SIMEBI  <  JHR-F07(1:6)
        COMPUTE  ZAI-F04  =  ZAI-F04  +  WK-JHR-F16 *>現在庫数
        COMPUTE  ZAI-F12  =  ZAI-F12  -  WK-JHR-F16 *>次月出庫数
        COMPUTE  ZAI-F13  =  ZAI-F13  -  WK-JHR-F16 *>次月売上数
        COMPUTE  ZAI-F14  =  ZAI-F14  +  WK-JHR-F16 *>次月返品数
     ELSE
        COMPUTE  ZAI-F04  =  ZAI-F04  +  WK-JHR-F16 *>現在庫数
        COMPUTE  ZAI-F06  =  ZAI-F06  +  WK-JHR-F16 *>当月入出数
        COMPUTE  ZAI-F08  =  ZAI-F08  -  WK-JHR-F16 *>当月出庫数
        COMPUTE  ZAI-F09  =  ZAI-F09  -  WK-JHR-F16 *>当月売上数
        COMPUTE  ZAI-F10  =  ZAI-F10  +  WK-JHR-F16 *>当月返品数
     END-IF.
*商品名称マスタ読込み
     PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
*2014.04.15↓
*    IF  HMEIMS-INV-FLG  =  SPACE
*        MOVE  173           TO   ZAI-F29
*        MOVE  MEI-F031      TO   ZAI-F30
*        MOVE  SYS-DATEW     TO   ZAI-F98
*        MOVE  SYS-DATEW     TO   ZAI-F99
*        WRITE ZAI-REC
*        MOVE  "1"           TO   DEN-F27C
*    END-IF.
     IF  HMEIMS-INV-FLG  =  SPACE
         MOVE  MEI-F031      TO   ZAI-F30
     END-IF.
     MOVE      173           TO   ZAI-F29.
     MOVE      SYS-DATEW     TO   ZAI-F98.
     MOVE      SYS-DATEW     TO   ZAI-F99.
     WRITE     ZAI-REC.
     MOVE      "1"           TO   DEN-F27C.
*2014.04.15↑
*
 ZAIKO-UPDATE1-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
*数量を変換
      MOVE      JHR-F16       TO   WK-JHR-F16.
*     次月の場合
     IF  WK-SIMEBI  <  JHR-F07(1:6)
        COMPUTE  ZAI-F04  =  ZAI-F04  +  WK-JHR-F16 *>現在庫数
        COMPUTE  ZAI-F12  =  ZAI-F12  -  WK-JHR-F16 *>次月出庫数
        COMPUTE  ZAI-F13  =  ZAI-F13  -  WK-JHR-F16 *>次月売上数
        COMPUTE  ZAI-F14  =  ZAI-F14  +  WK-JHR-F16 *>次月返品数
      ELSE
        COMPUTE  ZAI-F04  =  ZAI-F04  +  WK-JHR-F16 *>現在庫数
        COMPUTE  ZAI-F06  =  ZAI-F06  +  WK-JHR-F16 *>当月入出数
        COMPUTE  ZAI-F08  =  ZAI-F08  -  WK-JHR-F16 *>当月出庫数
        COMPUTE  ZAI-F09  =  ZAI-F09  -  WK-JHR-F16 *>当月売上数
        COMPUTE  ZAI-F10  =  ZAI-F10  +  WK-JHR-F16 *>当月返品数
      END-IF.
*2014.04.15↓
      MOVE  SYS-DATEW     TO   ZAI-F99.
*2014.04.15↑
*
*在庫マスタ更新
      REWRITE ZAI-REC.
      MOVE  "1"           TO   DEN-F27C.
*
 ZAIKO-UPDATE2-EXIT.
*-------------< PROGRAM END >------------------------------------*

```
