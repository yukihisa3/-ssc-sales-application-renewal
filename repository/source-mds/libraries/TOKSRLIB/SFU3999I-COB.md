# SFU3999I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SFU3999I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　　　　　　              *
*    業務名　　　　　　　：　マスタ保守                        *
*    モジュール名　　　　：　シーズンマスタ保守　              *
*    作成日／更新日　　　：　2016/12/21                        *
*    作成者／更新者　　　：　NAV INOUE                         *
*    処理概要　　　　　　：　シーズンマスタの保守を行う。      *
*    流用元　　　　　　　：　SIT0220I                          *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SFU3999I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2016/12/21.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*シーズンマスタ
     SELECT  SEASONF   ASSIGN    TO        DA-01-VI-SEASONL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       SEA-F01
                       FILE      STATUS    SEA-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = シーズンマスタ                                   *
****************************************************************
 FD  SEASONF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SEASONF   OF   XFDLIB
                       JOINING   SEA       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FFU39991  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SEA-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SEASONF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*システム日付変換用
  01  G-DATE.
     03  G-DATE-YY                PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  G-DATE-MM                PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  G-DATE-DD                PIC  9(02)  VALUE  ZERO.
*システム日付変換用
  01  G-TIME.
     03  G-TIME-HH                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-MM                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-SS                PIC  Z9.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(30)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(30)
             VALUE NC"_取消　_終了　_項目戻し".
     03  PF-MSG3.
         05  FILLER               PIC   N(30)   VALUE
     NC"_取消　_終了　_項目戻し　_前レコード　_次レコード".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   3   PIC   N(30).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(25)
             VALUE NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)
             VALUE NC"正しい値を入力して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)
             VALUE NC"シーズンコードを入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)
             VALUE NC"次レコードは存在しません。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)
             VALUE NC"シーズンマスタに登録済みです。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)
             VALUE NC"シーズンマスタに未登録です。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(25)
             VALUE NC"前レコードは存在しません。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(25)
*            VALUE NC"シーズンコードを入力して下さい。".
             VALUE NC"　　　　　　　　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  8   PIC   N(25).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SEA-ERR           PIC N(15) VALUE
         NC"シーズンマスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
         NC"画面ファイルエラー".
     03  IN-DATA           PIC X(01) VALUE SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
*
*レコード退避エリア
 01  DATA-TAIHI            PIC  X(510).
*
 01  SEQ                   PIC  9(02).
*
 LINKAGE                   SECTION.
 01  PARA-BUMONCD          PIC X(04).
 01  PARA-TANCD            PIC X(08).
*
**************************************************************
 PROCEDURE                 DIVISION  USING PARA-BUMONCD
                                           PARA-TANCD.
**************************************************************
 DECLARATIVES.
*
 SEA-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SEASONF.
     MOVE        SEA-ST    TO        E-ST.
     MOVE        "SEASONL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SEA-ERR   UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     ACCEPT      IN-DATA   FROM      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
*
     MOVE     "PROCESS START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
*
     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*    MOVE      SYS-DATE           TO   PARA-UPDTDATE.
*    MOVE      WK-TIME(1:6)       TO   PARA-UPDTIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       SEASONF   DSPFILE.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*画面初期化
         WHEN      "1"  PERFORM   DSP-INIT-SEC
*処理区分入力
         WHEN      "2"  PERFORM   DSP-HEAD1-SEC
*キー項目入力
         WHEN      "3"  PERFORM   DSP-HEAD2-SEC
*明細項目入力
         WHEN      "4"  PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "5"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                          *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FFU39991.
*システム日付転送
     MOVE    SYS-DATE(1:4)        TO   G-DATE-YY.
     MOVE    SYS-DATE(5:2)        TO   G-DATE-MM.
     MOVE    SYS-DATE(7:2)        TO   G-DATE-DD.
     MOVE    G-DATE               TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:2)         TO   G-TIME-HH.
     MOVE    WK-TIME(3:2)         TO   G-TIME-MM.
     MOVE    WK-TIME(5:2)         TO   G-TIME-SS.
     MOVE    G-TIME               TO   DSP-STIME.
*プログラムＩＤ
     MOVE    "SFU3999I"           TO   DSP-PGID.
*ＦＯＲＭＩＤ
     MOVE    "FFU39991"           TO   DSP-FORMID.
*リバース，カーソルパーク解除
*シーズンコード
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SEASCD.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SEASCD.
*シーズン名
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SEASNM.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SEASNM.
*シーズン名略
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SEASRY.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SEASRY.
*処理区分入力へ
     MOVE    "2"                  TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 2 )                2.1       *
****************************************************************
 DSP-HEAD1-SEC         SECTION.
     MOVE     "DSP-HEAD1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD1-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                       2.1.1     *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF       DSP-SYORI      NOT NUMERIC
     OR       DSP-SYORI      =    ZERO
              MOVE      2         TO   ERR-FLG
              GO                  TO   HEAD1-CHK-EXIT
     END-IF.
*処理区分＝１，２，３以外はエラー
     IF       DSP-SYORI      =    1   OR   2   OR   3
              MOVE     "3"        TO   PSW
*             同一モードでループさせるため
              MOVE     DSP-SYORI  TO   SAV-SHORI
     ELSE
              MOVE      2         TO   ERR-FLG
     END-IF.
*
 HEAD1-CHK-EXIT.
     EXIT.
****************************************************************
*             キー項目入力( PSW = 3 )                2.2       *
****************************************************************
 DSP-HEAD2-SEC         SECTION.
     MOVE     "DSP-HEAD2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD2-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*前レコード
         WHEN   "F011"
                IF   DSP-SYORI     NOT =    1
                     PERFORM   BEFORE-SEC
                ELSE
                     MOVE      1      TO   ERR-FLG
                END-IF
*次レコード
         WHEN   "F012"
                IF   DSP-SYORI     NOT =    1
                     PERFORM   NEXT-SEC
                ELSE
                     MOVE      1      TO   ERR-FLG
                END-IF
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*             キー項目チェック                       2.2.1     *
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     MOVE     "HEAD2-CHK-SEC"     TO   S-NAME.
*キー項目 未入力チェック
*シーズンコード
     IF       DSP-SEASCD  =  SPACE
              MOVE   3       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-SEASCD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-SEASCD
     END-IF.
*  エラーの場合は、ＥＸＩＴへ
     IF       ERR-FLG  NOT =  ZERO
              GO       TO     HEAD2-CHK-EXIT
     END-IF.
*  シーズンマスタ読込み
     MOVE     DSP-SEASCD     TO   SEA-F01.
     PERFORM  SEASONF-READ-SEC.
*    IF       SEASONF-INV-FLG NOT =  SPACE
*             MOVE   3       TO   ERR-FLG
*             MOVE  "R"      TO   EDIT-OPTION   OF  DSP-SEASCD
*             MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-SEASCD
*    END-IF.
*処理区分により判定
     EVALUATE DSP-SYORI
         WHEN  1
               IF   SEASONF-INV-FLG = SPACE
                    MOVE  5     TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-SEASCD
                    MOVE "C"    TO EDIT-CURSOR OF DSP-SEASCD
               ELSE
                    MOVE "4"    TO PSW
                    MOVE "M"    TO EDIT-OPTION OF DSP-SEASCD
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-SEASCD
               END-IF
         WHEN  2
               IF   SEASONF-INV-FLG = "INV"
                    MOVE  6     TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-SEASCD
                    MOVE "C"    TO EDIT-CURSOR OF DSP-SEASCD
                    MOVE  SPACE TO DSP-SEASNM  DSP-SEASRY
               ELSE
                    MOVE "4"    TO PSW
                    MOVE "M"    TO EDIT-OPTION OF DSP-SEASCD
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-SEASCD
                    PERFORM MST-DSP-SEC
               END-IF
         WHEN  3
               IF   SEASONF-INV-FLG = "INV"
                    MOVE  6     TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-SEASCD
                    MOVE "C"    TO EDIT-CURSOR OF DSP-SEASCD
                    MOVE  SPACE TO DSP-SEASNM  DSP-SEASRY
               ELSE
                    MOVE "5"    TO PSW
                    MOVE "M"    TO EDIT-OPTION OF DSP-SEASCD
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-SEASCD
                    PERFORM MST-DSP-SEC
               END-IF
     END-EVALUATE.
*
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*             前レコード表示                         2.3.1     *
****************************************************************
 BEFORE-SEC            SECTION.
     MOVE     "BEFORE-SEC"       TO   S-NAME.
*
*全レコード解放（排他制御用）
     CLOSE    SEASONF.
     OPEN     I-O   SEASONF.
* 属性クリア
     MOVE     SPACE     TO   SEASONF-INV-FLG.
     MOVE     " "       TO   EDIT-CURSOR OF DSP-SEASCD.
     MOVE     "M"       TO   EDIT-OPTION OF DSP-SEASCD.
*
     MOVE     DSP-SEASCD     TO  SEA-F01.
     START    SEASONF   KEY  IS  <  SEA-F01
                 WITH   REVERSED  ORDER
         INVALID  KEY
              MOVE  "INV"    TO  SEASONF-INV-FLG
         NOT  INVALID  KEY
              PERFORM  SEASONF-NEXTREAD-SEC
     END-START.
     IF  SEASONF-INV-FLG  =   "INV"
         MOVE      7    TO   ERR-FLG
         MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SEASCD
         MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SEASCD
         GO   TO   BEFORE-EXIT
     END-IF.
* 該当データを画面にセット
     PERFORM  MST-DSP-SEC.
*
 BEFORE-EXIT.
     EXIT.
****************************************************************
*             次レコード表示                         2.3.2     *
****************************************************************
 NEXT-SEC              SECTION.
     MOVE     "NEXT-SEC"         TO   S-NAME.
*
*全レコード解放（排他制御用）
     CLOSE    SEASONF.
     OPEN     I-O   SEASONF.
*
     MOVE     SPACE     TO   SEASONF-INV-FLG.
     MOVE     " "       TO   EDIT-CURSOR OF DSP-SEASCD.
     MOVE     "M"       TO   EDIT-OPTION OF DSP-SEASCD.
*
* シーズンコードチェック
     MOVE     DSP-SEASCD     TO  SEA-F01.
     START    SEASONF   KEY   >  SEA-F01
         INVALID  KEY
              MOVE     "INV"     TO   SEASONF-INV-FLG
         NOT  INVALID  KEY
              PERFORM  SEASONF-NEXTREAD-SEC
     END-START.
     IF  SEASONF-INV-FLG  =   "INV"
         MOVE      4    TO   ERR-FLG
         MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SEASCD
         MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SEASCD
         GO   TO   NEXT-EXIT
     END-IF.
* 該当データを画面にセット
     PERFORM MST-DSP-SEC.
*
 NEXT-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 4 )              2.3       *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    "5"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "3"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 5 ）            2.4
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                EVALUATE  DSP-SYORI
*-------------------登録
                    WHEN  "1"
                          PERFORM     FILE-WRT-SEC
*-------------------修正
                    WHEN  "2"
                          PERFORM     FILE-UPD-SEC
*-------------------削除
                    WHEN  "3"
                          PERFORM     FILE-DLT-SEC
                END-EVALUATE
                PERFORM   DSP-INIT-SEC
                MOVE    "3"      TO   PSW
                MOVE   SAV-SHORI TO   DSP-SYORI
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                IF  DSP-SYORI   =  1   OR   2
                    MOVE    "4"       TO   PSW
                ELSE
                    MOVE    "3"       TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    シーズンマスタ更新　　処理区分＝１（登録）2.4.1         *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
     MOVE     SPACE               TO   SEA-REC.
     INITIALIZE                        SEA-REC.
*キー項目転送
***  シーズンコード
     MOVE     DSP-SEASCD          TO   SEA-F01.
***  シーズン名（正式）
     MOVE     DSP-SEASNM          TO   SEA-F02.
***  シーズン名（略称）
     MOVE     DSP-SEASRY          TO   SEA-F03.
***  登録担当者部門ＣＤ
     MOVE     PARA-BUMONCD        TO   SEA-F92  SEA-F96.
***  登録担当者ＣＤ
     MOVE     PARA-TANCD          TO   SEA-F93  SEA-F97.
***  登録日付　　　　
     MOVE     SYS-DATE            TO   SEA-F94  SEA-F98.
***  登録時刻　　　　
     MOVE     WK-TIME(1:6)        TO   SEA-F95  SEA-F99.
*
*シーズンマスタ登録
     WRITE    SEA-REC.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*    シーズンマスタ更新　　処理区分＝２（修正）  2.4.2       *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE     "FILE-UPD-SEC"      TO   S-NAME.
*キー項目転送
***  取引先コード
     MOVE     DSP-SEASCD          TO   SEA-F01.
     PERFORM  SEASONF-READ-SEC.
     IF       SEASONF-INV-FLG = SPACE
*            シーズン名（正式）
              MOVE     DSP-SEASNM          TO   SEA-F02
*            シーズン名（略称）
              MOVE     DSP-SEASRY          TO   SEA-F03
*            更新担当者部門ＣＤ
              MOVE     PARA-BUMONCD        TO   SEA-F96
*            更新担当者ＣＤ
              MOVE     PARA-TANCD          TO   SEA-F97
*            更新日付
              MOVE     SYS-DATE            TO   SEA-F98
*            更新時刻
              MOVE     WK-TIME(1:6)        TO   SEA-F99
*
              REWRITE  SEA-REC
*
     ELSE
              DISPLAY "## SEASONF REWRITE ERR ##"  UPON CONS
              DISPLAY "## SEASON =" SEA-F01 " ##"  UPON CONS
              STOP  RUN
     END-IF.
*
 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*    シーズンマスタ更新　処理区分＝３（削除）    2.4.3       *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE     "FILE-DLT-SEC"      TO   S-NAME.
*キー項目転送
***  取引先コード
     MOVE     DSP-SEASCD            TO   SEA-F01.
     PERFORM  SEASONF-READ-SEC.
     IF       SEASONF-INV-FLG = SPACE
*            レコード削除
              DELETE   SEASONF
*
     ELSE
              DISPLAY "## SEASONF DELETE  ERR ##" UPON CONS
              DISPLAY "## SEASON =" SEA-F01 " ##" UPON CONS
              STOP  RUN
     END-IF.
*
 FILE-DLT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-MSG1
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSG1
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
*処理区分
         WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-MSG2
*キー項目／ボディー部／確認部
         WHEN   "3"   WHEN   "4"   WHEN   "5"
              IF   DSP-SYORI      =    1
                MOVE    PF-MSG-R(2)        TO   DSP-MSG2
              ELSE
                IF      PSW   =   "3"
                   MOVE    PF-MSG-R(3)     TO   DSP-MSG2
                ELSE
                   MOVE    PF-MSG-R(2)     TO   DSP-MSG2
                END-IF
              END-IF
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-MSG2
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FFU39991"          TO   DSP-FMT.
     WRITE    DSP-FFU39991.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
*    MOVE    "SCREEN"             TO   DSP-GRP.
     EVALUATE   PSW
*処理区分
         WHEN   "2"
                MOVE    "GPHEAD"  TO   DSP-GRP
*キー項目
         WHEN   "3"
                MOVE    "GPKEY"   TO   DSP-GRP
*明細項目
         WHEN   "4"
                MOVE    "GPBODY"  TO   DSP-GRP
*確認
         WHEN   "5"
                MOVE    "GPKAKU"  TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FFU39991"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             シーズンマスタ内容画面セット                   *
****************************************************************
 MST-DSP-SEC              SECTION.
     MOVE   "MST-DSP-SEC"         TO   S-NAME.
*
     MOVE    SEA-F01              TO   DSP-SEASCD.
     MOVE    SEA-F02              TO   DSP-SEASNM.
     MOVE    SEA-F03              TO   DSP-SEASRY.
*
 MST-DSP-EXIT.
     EXIT.
****************************************************************
*             シーズンマスタ読込み                           *
****************************************************************
 SEASONF-READ-SEC         SECTION.
     MOVE   "SEASONF-READ-SEC"    TO   S-NAME.
*
     READ    SEASONF
         INVALID
             MOVE    "INV"        TO   SEASONF-INV-FLG
         NOT INVALID
             MOVE     SPACE       TO   SEASONF-INV-FLG
     END-READ.
*
 SEASONF-READ-EXIT.
     EXIT.
****************************************************************
*             シーズンマスタ順読込み                         *
****************************************************************
 SEASONF-NEXTREAD-SEC     SECTION.
     MOVE   "SEASONF-NEXTREAD-SEC"     TO   S-NAME.
*
     READ    SEASONF    NEXT
         AT END
             MOVE    "INV"        TO   SEASONF-INV-FLG
         NOT AT END
             MOVE     SPACE       TO   SEASONF-INV-FLG
     END-READ.
*
 SEASONF-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             SEASONF  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SFU3999I   END PROGRAM  >>******************

```
