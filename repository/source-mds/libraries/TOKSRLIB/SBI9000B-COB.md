# SBI9000B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBI9000B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＢＩツール連携　　　　　　　　　　*
*    モジュール名　　　　：　日付管理マスタデータ作成　　　　　*
*    作成日／更新日　　　：　2018/12/17                        *
*    作成者／更新者　　　：　INOUE                             *
*    処理概要　　　　　　：　受け取ったパラメタ条件より、　　　*
*                          　日付管理マスタを作成する。        *
****************************************************************
 IDENTIFICATION      DIVISION.
 PROGRAM-ID.         SBI9000B.
 AUTHOR.             NAV.
 DATE-WRITTEN.       2018.12.17.
*
 ENVIRONMENT         DIVISION.
 CONFIGURATION       SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS     CONS.
*
 INPUT-OUTPUT        SECTION.
 FILE-CONTROL.
*日付管理マスタ
     SELECT   DATECKL1       ASSIGN        TO  01-VI-DATECKL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS  HIZ-F01
                             FILE STATUS   IS  HIZ-STA.

*
**************************************************************
 DATA                DIVISION.
**************************************************************
*=============================================================
 FILE                SECTION.
*=============================================================
*日付管理マスタ
 FD  DATECKL1.
     COPY     DATECKL1  OF  XFDLIB
     JOINING  HIZ       AS  PREFIX.
*=============================================================
 WORKING-STORAGE     SECTION.
*=============================================================
*ステータス
 01  STA-AREA.
     03  HIZ-STA             PIC  X(02).
*カウント
 01  CNT-AREA.
     03  IN-CNT                   PIC  9(07)  VALUE  ZERO.
     03  OT-CNT                   PIC  9(07)  VALUE  ZERO.
     03  RW-CNT                   PIC  9(07)  VALUE  ZERO.
*ＦＬＧ
 01  WK-FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  FLG-HIT                  PIC  X(03)  VALUE  SPACE.
     03  FLG-INV                  PIC  X(03)  VALUE  SPACE.
*推移日付
 01  WK-DATA-AREA.
     03  WK-UP-HIDUKE             PIC  9(08)  VALUE  ZERO.
     03  WK-UP-HIDUKE-X           REDEFINES   WK-UP-HIDUKE.
         05 WK-UP-HIDUKE-X-YYYY   PIC  X(04).
         05 WK-UP-HIDUKE-X-MMDD   PIC  X(04).
     03  WK-UP-YOUBI              PIC  9(01)  VALUE  ZERO.
     03  WK-UP-SYU                PIC  9(02)  VALUE  ZERO.
     03  WK-UP-KI                 PIC  9(03)  VALUE  ZERO.
     03  WK-UP-HIDUKE-END         PIC  9(08)  VALUE  ZERO.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  WK-SDATE                PIC  9(08).
 01  WK-SDATE-R   REDEFINES   WK-SDATE.
     03  FILLER              PIC  X(02).
     03  WK-SYMD.
       05  WK-SYY            PIC  9(02).
       05  WK-SMM            PIC  9(02).
       05  WK-SDD            PIC  9(02).
 01  WK-EDATE                PIC  9(08).
 01  WK-EDATE-R   REDEFINES   WK-EDATE.
     03  FILLER              PIC  X(02).
     03  WK-EYMD.
       05  WK-EYY            PIC  9(02).
       05  WK-EMM            PIC  9(02).
       05  WK-EDD            PIC  9(02).
*メッセージ情報
 01  MSG-AREA.
     03  MSG-ABEND1.
         05  FILLER          PIC  X(12)  VALUE  "### SBI9000B".
         05  FILLER          PIC  X(11)  VALUE  "  ABEND ###".
     03  MSG-ABEND2.
         05  FILLER          PIC  X(04)  VALUE  "### ".
         05  ERR-FL-ID       PIC  X(08).
         05  FILLER          PIC  X(04)  VALUE  " ST-".
         05  ERR-STCD        PIC  X(02).
         05  FILLER          PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*=============================================================
 LINKAGE             SECTION.
*=============================================================
   01  LINK-HIDUKE-FROM      PIC  9(08).
   01  LINK-YOUBI            PIC  9(01).
   01  LINK-KI               PIC  9(03).
   01  LINK-HIDUKE-END       PIC  9(08).
******************************************************************
 PROCEDURE           DIVISION  USING   LINK-HIDUKE-FROM
                                       LINK-YOUBI
                                       LINK-KI
                                       LINK-HIDUKE-END.
******************************************************************
 DECLARATIVES.
*日付管理マスタ
 HIZ-ERR-SEC        SECTION.
     USE      AFTER  EXCEPTION   PROCEDURE       DATECKL1.
     MOVE    "DATECKL1"    TO    ERR-FL-ID.
     MOVE     HIZ-STA      TO    ERR-STCD.
     DISPLAY  MSG-ABEND1   UPON  CONS.
     DISPLAY  MSG-ABEND2   UPON  CONS.
     MOVE     4000         TO    PROGRAM-STATUS.
     STOP     RUN.
 END  DECLARATIVES.
*=============================================================
*               コントロール
*=============================================================
 CONTROL-SEC         SECTION.
     DISPLAY  "**  SBI9000B   START  **"   UPON  CONS.
*
     PERFORM  IHIZ-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
     DISPLAY  "**  SBI9000B    END   **"   UPON  CONS.
     STOP  RUN.
 CONTROL-EXIT.
     EXIT.
*=============================================================
*               初期処理
*=============================================================
 IHIZ-SEC            SECTION.
*ファイル ＯＰＥＮ
     OPEN     I-O         DATECKL1.
*
 IHIZ-01.
*許容開始日チェック
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     ZERO                TO   LINK-IN-YMD6.
     MOVE     LINK-HIDUKE-FROM    TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
              CONTINUE
     ELSE
              DISPLAY NC"指定基準日エラー" UPON CONS
              MOVE  4010  TO    PROGRAM-STATUS
              STOP  RUN
     END-IF.
*許容終了日チェック
     MOVE     "2"                 TO   LINK-IN-KBN.
     MOVE     ZERO                TO   LINK-IN-YMD6.
     MOVE     LINK-HIDUKE-END     TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
              CONTINUE
     ELSE
              DISPLAY NC"指定終了日エラー" UPON CONS
              MOVE  4010  TO    PROGRAM-STATUS
              STOP  RUN
     END-IF.
*許容曜日チェック
     IF     ( LINK-YOUBI  NUMERIC ) AND
            ( LINK-YOUBI  >  0    ) AND ( LINK-YOUBI < 8 )
              CONTINUE
     ELSE
              DISPLAY NC"指定曜日エラー（１～７以外）" UPON CONS
              MOVE  4010  TO    PROGRAM-STATUS
              STOP  RUN
     END-IF.
*許容期チェック
     IF       LINK-KI NUMERIC
              CONTINUE
     ELSE
              DISPLAY NC"指定期エラー（数値以外）" UPON CONS
              MOVE  4010  TO    PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
     MOVE     LINK-HIDUKE-FROM TO    WK-UP-HIDUKE.
     MOVE     LINK-YOUBI       TO    WK-UP-YOUBI.
     MOVE     1                TO    WK-UP-SYU.
*
 IHIZ-02.
*基準日の週番号を特定する
*　基準日＝１月１日なら１週目
     IF       WK-UP-HIDUKE-X-MMDD = "0101"
              GO              TO     IHIZ-04
     END-IF.
*
 IHIZ-03.
*　同年１月１日まで遡る
*　　－１日取得
     PERFORM     UNTIL   FLG-HIT     =  "HIT"
         MOVE       "6"           TO     LINK-IN-KBN
         MOVE        1            TO     LINK-IN-YMD6
         MOVE        WK-UP-HIDUKE TO     LINK-IN-YMD8
         CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                         LINK-IN-YMD6
                                         LINK-IN-YMD8
                                         LINK-OUT-RET
                                         LINK-OUT-YMD
         IF          LINK-OUT-RET   =    ZERO
             MOVE    LINK-OUT-YMD   TO   WK-UP-HIDUKE
         ELSE
             DISPLAY "## PROGRAM ABEND ##"  UPON CONS
             STOP  RUN
         END-IF
*
         COMPUTE   WK-UP-YOUBI     =   WK-UP-YOUBI - 1
         IF        WK-UP-YOUBI     =   1
                   MOVE     7      TO  WK-UP-YOUBI
         ELSE
                   COMPUTE WK-UP-YOUBI         =   WK-UP-YOUBI - 1
                   IF      WK-UP-YOUBI         =   1
                           COMPUTE  WK-UP-SYU  =   WK-UP-SYU + 1
                   END-IF
         END-IF

         IF        WK-UP-HIDUKE-X-MMDD = "0101"
                   MOVE     "HIT"  TO  FLG-HIT
         END-IF
     END-PERFORM.
*
*
*    MOVE       "6"           TO     LINK-IN-KBN.
*    MOVE        1            TO     LINK-IN-YMD6.
*    MOVE        WK-UP-HIDUKE TO     LINK-IN-YMD8.
*    CALL       "SKYDTCKB"   USING   LINK-IN-KBN
*                                    LINK-IN-YMD6
*                                    LINK-IN-YMD8
*                                    LINK-OUT-RET
*                                    LINK-OUT-YMD.
*    IF          LINK-OUT-RET   =    ZERO
*        MOVE    LINK-OUT-YMD   TO   WK-UP-HIDUKE
*    ELSE
*        DISPLAY "## PROGRAM ABEND ##"  UPON CONS
*        STOP  RUN
*    END-IF.
*
*    COMPUTE   WK-UP-YOUBI     =   WK-UP-YOUBI - 1.
*    IF        WK-UP-YOUBI     =   1
*              MOVE     7      TO  WK-UP-YOUBI
*    ELSE
*              COMPUTE  WK-UP-YOUBI         =   WK-UP-YOUBI -  1
*              IF       WK-UP-YOUBI         =   1
*                       COMPUTE  WK-UP-SYU  =   WK-UP-SYU   +  1
*              END-IF
*    END-IF.
*
*    IF        WK-UP-HIDUKE-X-MMDD = "0101"
*              MOVE     "HIT"  TO  FLG-HIT
*              GO              TO  IHIZ-04
*    ELSE
*              GO              TO  IHIZ-03
*    END-IF.
*
 IHIZ-04.
*パラメタ再セット
     MOVE      LINK-HIDUKE-FROM   TO    WK-UP-HIDUKE.
     MOVE      LINK-YOUBI         TO    WK-UP-YOUBI.
     MOVE      LINK-KI            TO    WK-UP-KI.
     MOVE      LINK-HIDUKE-END    TO    WK-UP-HIDUKE-END.
*
*１件目ＷＲＩＴＥ
     MOVE    SPACE                TO    HIZ-REC.
     INITIALIZE                         HIZ-REC.
*存在チェック
     MOVE    WK-UP-HIDUKE         TO    HIZ-F01.
     READ    DATECKL1
       INVALID
             MOVE    "INV"        TO    FLG-INV
       NOT INVALID
             MOVE    "   "        TO    FLG-INV
     END-READ.
*日付
     MOVE    WK-UP-HIDUKE         TO    HIZ-F01.
*曜日_
     MOVE    WK-UP-YOUBI          TO    HIZ-F02.
*曜日名
     EVALUATE HIZ-F02
         WHEN 1  MOVE NC"月"      TO    HIZ-F03
         WHEN 2  MOVE NC"火"      TO    HIZ-F03
         WHEN 3  MOVE NC"水"      TO    HIZ-F03
         WHEN 4  MOVE NC"木"      TO    HIZ-F03
         WHEN 5  MOVE NC"金"      TO    HIZ-F03
         WHEN 6  MOVE NC"土"      TO    HIZ-F03
         WHEN 7  MOVE NC"日"      TO    HIZ-F03
     END-EVALUATE.
*週番
     MOVE    WK-UP-SYU             TO   HIZ-F04.
*計上年月
     MOVE    WK-UP-HIDUKE(1:6)     TO   HIZ-F05.
*期
     MOVE    WK-UP-KI              TO   HIZ-F06.
*
*
     IF      FLG-INV  =  "INV"
             WRITE    HIZ-REC
             ADD      1         TO   OT-CNT
     ELSE
             REWRITE  HIZ-REC
             ADD      1         TO   RW-CNT
     END-IF.
*
     IF      WK-UP-HIDUKE  =    WK-UP-HIDUKE-END
             MOVE  "END"        TO   END-FLG
     END-IF.
*
 IHIZ-EXIT.
     EXIT.
*=============================================================
*                メイン処理
*=============================================================
 MAIN-SEC            SECTION.
*＋１日取得
     MOVE       "5"           TO     LINK-IN-KBN.
     MOVE        1            TO     LINK-IN-YMD6.
     MOVE        WK-UP-HIDUKE TO     LINK-IN-YMD8.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD   TO   WK-UP-HIDUKE
     ELSE
         DISPLAY "## PROGRAM ABEND ##"  UPON CONS
         STOP  RUN
     END-IF.
*
     ADD     1                  TO   WK-UP-YOUBI.
*
     IF      WK-UP-YOUBI  >  7
             MOVE     1         TO   WK-UP-YOUBI
     END-IF.
*
     MOVE    SPACE              TO   HIZ-REC.
     INITIALIZE                      HIZ-REC.
*存在チェック
     MOVE    WK-UP-HIDUKE       TO   HIZ-F01.
     READ    DATECKL1
       INVALID
             MOVE    "INV"      TO   FLG-INV
       NOT INVALID
             MOVE    "   "      TO   FLG-INV
     END-READ.
*日付
     MOVE    WK-UP-HIDUKE       TO   HIZ-F01.
*曜日_
     MOVE    WK-UP-YOUBI        TO   HIZ-F02.
*曜日名
     EVALUATE HIZ-F02
         WHEN 1  MOVE NC"月"    TO   HIZ-F03
         WHEN 2  MOVE NC"火"    TO   HIZ-F03
         WHEN 3  MOVE NC"水"    TO   HIZ-F03
         WHEN 4  MOVE NC"木"    TO   HIZ-F03
         WHEN 5  MOVE NC"金"    TO   HIZ-F03
         WHEN 6  MOVE NC"土"    TO   HIZ-F03
         WHEN 7  MOVE NC"日"    TO   HIZ-F03
     END-EVALUATE.
*週番
     IF      WK-UP-YOUBI         =   1
             COMPUTE  WK-UP-SYU  =   WK-UP-SYU  +  1
     END-IF.
     IF      WK-UP-HIDUKE-X-MMDD =   "0101"
             MOVE     1          TO  WK-UP-SYU
     END-IF.
     MOVE    WK-UP-SYU           TO  HIZ-F04.
*計上年月
     MOVE    WK-UP-HIDUKE(1:6)   TO  HIZ-F05.
*期
     IF      WK-UP-HIDUKE-X-MMDD =   "0601"
             COMPUTE  WK-UP-KI   =   WK-UP-KI   +  1
     END-IF.
     MOVE    WK-UP-KI           TO   HIZ-F06.
*
*
     IF      FLG-INV  =  "INV"
             WRITE    HIZ-REC
             ADD      1         TO   OT-CNT
     ELSE
             REWRITE  HIZ-REC
             ADD      1         TO   RW-CNT
     END-IF.
*
     IF      WK-UP-HIDUKE  =    WK-UP-HIDUKE-END
             MOVE  "END"        TO   END-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*=============================================================
*      3.0        終了処理
*=============================================================
 END-SEC             SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      DATECKL1.
     DISPLAY NC"作成件数" " = " OT-CNT UPON CONS.
     DISPLAY NC"更新件数" " = " RW-CNT UPON CONS.
 END-EXIT.
     EXIT.

```
