# SSY3906L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY3906L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ホーマックオンライン　　　        *
*    モジュール名　　　　：　ＴＣ納品（納品シール発行）        *
*    作成日／更新日　　　：　01/03/09                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　専用用紙（ＴＹＰＥーＡ）          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY3906L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/03/09.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA             IS        YA
         YB             IS        YB
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ホーマック伝票データ（共通）
     SELECT   HMSHIRED  ASSIGN    TO        HMSHIRED
                        ACCESS    MODE IS   SEQUENTIAL
                        STATUS              DEN-ST.
*画面定義ファイル　　
     SELECT   DSPFILE   ASSIGN    TO        GS-DSPF
                        FORMAT              DSP-FMT
                        GROUP               DSP-GRP
                        PROCESSING          DSP-PRO
                        FUNCTION            DSP-FNC
                        STATUS              DSP-ST.
*プリント定義ファイル　　
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF
                        STATUS              PRT-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
****************************************************************
*  FILE =ホーマック伝票データ（共通）                          *
****************************************************************
 FD  HMSHIRED
                        BLOCK    CONTAINS  1    RECORDS
                        LABEL    RECORD    IS   STANDARD.
                        COPY     HMSHIRED OF   XFDLIB
                        JOINING  DEN       AS   PREFIX.
****************************************************************
*    FILE = ｶﾞﾒﾝ  F                                            *
****************************************************************
 FD  DSPFILE
                        LABEL RECORD   IS   OMITTED.
*
     COPY    FSY39061   OF   XMDLIB.
*
****************************************************************
*    FILE = ﾌﾟﾘﾝﾄ ﾌｱｲﾙ                                         *
****************************************************************
 FD  PRTFILE
                        LABEL RECORD   IS   OMITTED
                        LINAGE IS      15   LINES
                        DATA  RECORD   IS   PRT-REC.
 01  PRT-REC.
     03  FILLER                   PIC  X(200).
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
***  ｽﾃｰﾀｽｴﾘｱ
 01  STATUS-AREA.
     03  DEN-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
***  ｶﾞﾒﾝ ｺﾝﾄﾛｰﾙ ｴﾘｱ
 01  DSP-CNTL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
     03  DSP-ST                   PIC  X(02).
***  ﾌﾗｸﾞ ｴﾘｱ
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)     VALUE  ZERO.
     03  PROGRAM-END              PIC  X(03)     VALUE  ZERO.
     03  ERR-FLG                  PIC  9(01)     VALUE  ZERO.
     03  KAI-FLG                  PIC  9(01)     VALUE  ZERO.
     03  SYU-FLG                  PIC  9(01)     VALUE  ZERO.
     03  ZEN-FLG                  PIC  9(01)     VALUE  ZERO.
     03  TS-FLG                   PIC  9(01)     VALUE  ZERO.
     03  TEI-FLG                  PIC  9(01)     VALUE  ZERO.
***  ｶｳﾝﾄ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(07)     VALUE  ZERO.
     03  CNT-AFTER                PIC  9(07)     VALUE  ZERO.
     03  PAGE-CNT                 PIC  9(04)     VALUE  ZERO.
***  ｺﾞｳｹｲ
 01  GOUKEI.
     03  GENKINKEI                PIC  9(09)     VALUE  ZERO.
     03  BAIKINKEI                PIC  9(09)     VALUE  ZERO.
     03  SURYOKEI                 PIC  9(06)V9   VALUE  ZERO.
     03  TSGENKEI                 PIC  9(09)     VALUE  ZERO.
     03  TSBAIKEI                 PIC  9(09)     VALUE  ZERO.
     03  TSSURYOKEI               PIC  9(06)V9   VALUE  ZERO.
***  ﾜｰｸ ｴﾘｱ
 01  WRK-AREA.
     03  WK-MAI                   PIC  9(06)     VALUE  ZERO.
     03  RD-SW                    PIC  9(01)     VALUE  ZERO.
     03  SYS-DATE                 PIC  9(6)      VALUE  ZERO.
     03  SYS-TIME                 PIC  9(8)      VALUE  ZERO.
     03  IN-DATA                  PIC  X(01).
     03  DEN-NO                   PIC  9(09)     VALUE  ZERO.
     03  CNT-DENPYO               PIC  9(07)     VALUE  ZERO.
     03  ZERO-FLG                 PIC  9(01)     VALUE  ZERO.
     03  I                        PIC  9(02)     VALUE  ZERO.
     03  J                        PIC  9(02)     VALUE  ZERO.
     03  IX                       PIC  9(01)     VALUE  ZERO.
     03  WK-DEN-NO                PIC  9(09)     VALUE  ZERO.
     03  WK-DEN-A181              PIC  X(05)     VALUE  SPACE.
     03  WK-DEN-A12               PIC  9(04)     VALUE  ZERO.
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
***  ﾃﾞﾝﾋﾟﾖｳ ｷ-
 01  WRK-DENNO.
     03  DENNO1                   PIC  9(09).
***  連番変換
 01  WRK-HEN.
     03  WRK-HEN01                PIC  X(01).
     03  WRK-HEN011               PIC  X(05).
     03  WRK-HEN02                PIC  9(04).
     03  WRK-HEN021               PIC  X(01).
     03  WRK-HEN03                PIC  X(01).
***  ﾒｯｾｰｼﾞｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"　　　　　　　　　　　　　　　".
         05  MSG02                PIC  N(30)     VALUE
         NC"正しい番号を入力してください。".
         05  MSG03                PIC  N(30)     VALUE
         NC"仕入伝票発行中".
         05  MSG04                PIC  N(30)     VALUE
         NC"無効キーです".
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     4      PIC  N(30).
*
 01  FILE-ERR010                  PIC  N(15)     VALUE
         NC"プリンター　異常！！".
 01  FILE-ERR020                  PIC  N(15)     VALUE
         NC"仕入ファイル　異常！！".
 01  FILE-ERR030                  PIC  N(15)     VALUE
         NC"画面ファイル　異常！！".
***  ﾍﾝｼｭｳ ｴﾘｱ
 01  WK-SURYO                     PIC  9(05)V9.
 01  WK-SURYO-R  REDEFINES  WK-SURYO.
     03  WK-SURYO1                PIC  9(05).
     03  WK-SURYO2                PIC  9(01).
 01  WK-GENKA                     PIC  9(06)V99.
 01  WK-GENKA-R  REDEFINES  WK-GENKA.
     03  WK-GENKA1                PIC  9(06).
     03  WK-GENKA2                PIC  9(02).
 01  WK-SYOCD                     PIC  X(13).
 01  WK-SYOCD-R  REDEFINES  WK-SYOCD.
     03  WK-HEN-SYOCD             PIC  9(13).
***  画面用日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1             PIC  9(02).
     03  WK-DATE8-YY2             PIC  9(06).
***  日付変換
 01  WK-HACDYMD.
     03  WK-HACDYMD-1             PIC  9(02)   VALUE  ZERO.
     03  WK-HACDYMD-2             PIC  X(01)   VALUE  SPACE.
     03  WK-HACDYMD-3             PIC  9(02)   VALUE  ZERO.
     03  WK-HACDYMD-4             PIC  X(01)   VALUE  SPACE.
     03  WK-HACDYMD-5             PIC  9(02)   VALUE  ZERO.
***  出力取引先ＣＤ
 01  WK-TOKCD                     PIC  9(08)   VALUE  880.
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  GYO0.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   OCCURS  3.
       05  FILLER                 PIC  X(23)     VALUE   SPACE.
       05  PAGE-NO                PIC  ZZZZ.
       05  FILLER                 PIC  X(17)     VALUE   SPACE.
 01  GYO1                         CHARACTER  TYPE  IS  YB.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   OCCURS  3.
         05  HACDYMD              PIC  X(08).
         05  FILLER               PIC  X(01)     VALUE   SPACE.
         05  NOUDYMD              PIC  X(08).
         05  FILLER               PIC  X(01)     VALUE   SPACE.
         05  HATYUK               PIC  N(06).
         05  NOUNM                PIC  X(15)     VALUE   SPACE.
         05  FILLER               PIC  X(02)     VALUE   SPACE.
*
 01  GYO2.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   OCCURS  3.
         05  FILLER               PIC  X(03)     VALUE   SPACE.
         05  TOKCD                PIC  999999.
         05  TOKNM                PIC  X(32).
         05  FILLER               PIC  X(03)     VALUE   SPACE.
*
 01  GYO3.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   OCCURS  3.
         05  SYONM1               PIC  X(20).
         05  FILLER               PIC  X(24).
*
 01  GYO4.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   OCCURS  3.
         05  SYONM2               PIC  X(20).
         05  FILLER               PIC  X(24).
*
 01  GYO5.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   OCCURS  3.
         05  FILLER               PIC  X(01)     VALUE   SPACE.
         05  DEN                  PIC  9999999.
         05  FILLER               PIC  X(01)     VALUE   SPACE.
         05  GYOU                 PIC  99.
         05  FILLER               PIC  X(01)     VALUE   SPACE.
         05  JAN                  PIC  ZZZZZ99999999.
         05  FILLER               PIC  X(01)     VALUE   SPACE.
         05  TEISEI               PIC  ZZZ,ZZ9.
         05  HATYU1               PIC  ZZZZZZZZ9.
         05  HATYU2               PIC  Z.
         05  FILLER               PIC  X(01)     VALUE   SPACE.
*
 01  GYO5-TEISEI.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   OCCURS  3.
         05  FILLER               PIC  X(33)     VALUE   SPACE.
         05  TEISEI-GYOU          PIC  X(10)     VALUE   SPACE.
         05  FILLER               PIC  X(01)     VALUE   SPACE.
*
 01  GYO6.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
     03  FILLER                   OCCURS  3.
         05  FILLER               PIC  X(02)     VALUE   SPACE.
         05  BUMON                PIC  999.
         05  STANI                PIC  ZZZZZ9.
         05  BAIKA                PIC  ZZZZZZ9.
         05  FILLER               PIC  X(26)     VALUE   SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
 DECLARATIVES.
*--- << プリンタエラー >> ---*
 000-PRTFILE-ERR        SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      PRTFILE.
     DISPLAY  FILE-ERR010         UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     DISPLAY  PRT-ST              UPON      CONS.
     ACCEPT   IN-DATA             FROM      CONS.
     MOVE     "4000"              TO        PROGRAM-STATUS.
     STOP     RUN.
*--- << 仕入データエラー >> ---*
 000-MAST-ERR           SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      HMSHIRED.
     DISPLAY  FILE-ERR020         UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     DISPLAY  DEN-ST              UPON      CONS.
     ACCEPT   IN-DATA             FROM      CONS.
     MOVE     "4000"              TO        PROGRAM-STATUS.
     STOP     RUN.
*--- << 画面エラー >> ---*
 000-DSP-ERR            SECTION.
     USE AFTER          EXCEPTION      PROCEDURE      DSPFILE.
     DISPLAY  FILE-ERR030         UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     DISPLAY  DSP-ST              UPON      CONS.
     ACCEPT   IN-DATA             FROM      CONS.
     MOVE     "4000"              TO        PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*           　M A I N             M O D U L E         0.0      *
****************************************************************
 PROCESS-START          SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     PROGRAM-END    =   "END".
     PERFORM  END-SEC.
     STOP     RUN.
****************************************************************
*             初期処理                                1.0      *
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     HMSHIRED.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
*    初期値セット
     MOVE     ZERO                TO   DENNO1.
     MOVE     ZERO                TO   CNT-DENPYO.
     MOVE     SPACE               TO   END-FLG.
     MOVE     SPACE               TO   FSY39061.
*    伝票総枚数読込み
     PERFORM  DEN-CNT-SEC  UNTIL  END-FLG = "END".
*    伝票枚数チェック
     IF       WK-MAI  <=  ZERO
              DISPLAY NC"対象データ無し" UPON CONS
              MOVE    "END"      TO   PROGRAM-END
              GO                 TO   INIT-EXIT
     END-IF.
*    終了フラグクリア
     MOVE     SPACE              TO   END-FLG.
     CLOSE    HMSHIRED.
*
     OPEN     INPUT      HMSHIRED.
 INIT-EXIT.
     EXIT.
****************************************************************
*             伝票データファイル読込み                2.0      *
****************************************************************
 DEN-CNT-SEC            SECTION.
*
     READ     HMSHIRED  AT  END
              MOVE     "END"     TO   END-FLG
              GO                 TO   DEN-CNT-EXIT
     END-READ.
*    ホーマック以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  WK-TOKCD
              GO                 TO   DEN-CNT-SEC
     END-IF.
*    納品区分＝”１”（センター）以外読み飛ばし
*****DISPLAY "DEN-A15 = " DEN-A15 UPON CONS.
     IF       DEN-A15  NOT =  1
              GO                 TO   DEN-CNT-SEC
     END-IF.
*    伝票枚数カウント
     ADD      1                  TO   WK-MAI.
*
 DEN-CNT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                              2.0      *
****************************************************************
 MAIN-SEC               SECTION.
     MOVE          "MAIN-SEC"          TO   S-NAME.
*
     MOVE          ZERO                TO   PAGE-CNT.
     MOVE          SPACE               TO   FSY39061.
     PERFORM       DSP-WRITE-SEC.
*
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*
     MOVE          "MD04R"             TO   DSP-GRP.
     PERFORM       DSP-READ-SEC.
*
     EVALUATE      DSP-FNC
         WHEN      "F005"
                   MOVE     "END"      TO   PROGRAM-END
                   GO                  TO   MAIN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   MAIN-SEC
     END-EVALUATE.
*
     MOVE          ZERO                TO   IX.
     IF            MD04      NOT  NUMERIC
                   MOVE      2         TO   ERR-FLG
                   GO                  TO   MAIN-SEC
     END-IF.
*
     EVALUATE      MD04
         WHEN      1
                   PERFORM   TEST-PRT-SEC
         WHEN      2
                   MOVE      SPACE     TO   END-FLG
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
***                伝票データ発行中メッセージ表示
                   MOVE     3          TO   ERR-FLG
                   PERFORM   ERR-MSG-SEC
                   PERFORM   DSP-WRITE-SEC
***
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     END-FLG   =   "END"
                   IF       IX  >  ZERO
                            PERFORM NOUHIN-OUT-SEC
                            MOVE ZERO  TO   IX
                   END-IF
                   CLOSE     HMSHIRED  PRTFILE
                   OPEN      INPUT     HMSHIRED
                   OPEN      OUTPUT    PRTFILE
                   MOVE      1         TO   ERR-FLG
         WHEN      3
         WHEN      4
                   MOVE      SPACE     TO   END-FLG
                   IF   MD04 =    3
                        PERFORM   KEY-IN-SEC
                   ELSE
                        PERFORM   KEY-IN-SEC-2
                        MOVE 1    TO   ZEN-FLG
                   END-IF
***                伝票データ発行中メッセージ表示
                   IF       END-FLG  NOT = "END"
                            MOVE     3          TO   ERR-FLG
                            PERFORM   ERR-MSG-SEC
                            PERFORM   DSP-WRITE-SEC
                   END-IF
***
                   PERFORM   FL-READ-SEC
                   PERFORM   DENP-WT-SEC
                             UNTIL     END-FLG   =   "END"
                   IF       IX  >  ZERO
                            PERFORM NOUHIN-OUT-SEC
                            MOVE ZERO  TO   IX
                   END-IF
                   CLOSE     HMSHIRED  PRTFILE
                   OPEN      INPUT     HMSHIRED
                   OPEN      OUTPUT    PRTFILE
                   MOVE      1         TO   ERR-FLG
         WHEN      OTHER
                   MOVE      2         TO   ERR-FLG
                   GO                  TO   MAIN-SEC
     END-EVALUATE.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
     MOVE          ZERO           TO   DEN-NO
                                       KAI-FLG
                                       SYU-FLG
                                       ZEN-FLG
                                       RD-SW.
     MOVE          SPACE          TO   END-FLG.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理                            2.1      *
****************************************************************
 KEY-IN-SEC             SECTION.
     MOVE          "KEI-IN"       TO   S-NAME.
*
 KEY-IN-01.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*
     MOVE         "NE"            TO   DSP-PRO.
     MOVE         "KEY01"         TO   DSP-GRP.
     MOVE          ZERO           TO   MD02      MD03.
*
     PERFORM       DSP-READ-SEC.
*
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   KEY-IN-EXIT
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   KEY-IN-01
     END-EVALUATE.
*
     IF           (MD02   =  ZERO)     AND
                  (MD03   =  ZERO)
                   MOVE      ZERO      TO   MD02
                   MOVE      ALL "9"   TO   MD03
                   MOVE      1         TO   ZEN-FLG
     END-IF.
*
 KEY-IN-EXIT.
     EXIT.
****************************************************************
*             範囲指定処理２                          2.1      *
****************************************************************
 KEY-IN-SEC-2           SECTION.
     MOVE          "KEI-IN-2"     TO   S-NAME.
*
 KEY-IN-01-2.
     PERFORM       ERR-MSG-SEC.
     PERFORM       DSP-WRITE-SEC.
*
     MOVE         "NE"            TO   DSP-PRO.
     MOVE         "KEY02"         TO   DSP-GRP.
     MOVE          ZERO           TO   PGST      PGED.
*
     PERFORM       DSP-READ-SEC.
*
     EVALUATE      DSP-FNC
         WHEN     "F005"
                   MOVE     "END"      TO   END-FLG
                   GO                  TO   KEY-IN-EXIT-2
         WHEN     "E000"
                   CONTINUE
         WHEN      OTHER
                   MOVE      4         TO   ERR-FLG
                   GO                  TO   KEY-IN-01-2
     END-EVALUATE.
*
     IF           (PGST   =  ZERO)     AND
                  (PGED   =  ZERO)
                   MOVE      ZERO      TO   PGST
                   MOVE      ALL "9"   TO   PGED
                   MOVE      1         TO   ZEN-FLG
     END-IF.
*
     IF           (PGED   =  ZERO)
                   MOVE      ALL "9"   TO   PGED
     END-IF.
*
 KEY-IN-EXIT-2.
     EXIT.
****************************************************************
*             ファイルＲＥＡＤ処理                    2.2      *
****************************************************************
 FL-READ-SEC            SECTION.
     MOVE     "FL-READ"           TO        S-NAME.
*
 READ-000.
     READ     HMSHIRED    AT      END
              MOVE       "END"    TO        END-FLG
              MOVE        1       TO        ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO        FL-READ-EXIT
     END-READ.
*行_０の時
     IF       DEN-A03 =    ZERO
              GO                  TO        READ-000
     END-IF.
*    ホーマック以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  WK-TOKCD
              GO                 TO   FL-READ-SEC
     END-IF.
*    納品区分＝”１”（センター）以外読み飛ばし
     IF       DEN-A15  NOT =  1
              GO                 TO   FL-READ-SEC
     END-IF.
*MD04(印字パターン)=全件数印刷時
     IF       MD04    =    2  OR  ZEN-FLG   = 1
              GO                  TO        FL-READ-EXIT
     END-IF.
*    印字開始ＦＬＧ＝ＯＮの時
     IF       KAI-FLG   =    1    GO  TO    READ-010.
*    開始印字位置判定
     IF       DEN-F02   =    MD02
              MOVE      1         TO        KAI-FLG
     ELSE
              GO                  TO        FL-READ-SEC
     END-IF.
 READ-010.
*    最終終了判定
     IF       ( SYU-FLG     = 1      )  AND
              ( DEN-F02 NOT = DEN-NO )
              MOVE      "END"     TO        END-FLG
              GO                  TO        FL-READ-EXIT
     END-IF.
*    終了伝票番号判定
     IF       DEN-F02   =    MD03
              MOVE      DEN-F02   TO        DEN-NO
              MOVE      1         TO        SYU-FLG
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             伝票出力処理                            2.3      *
****************************************************************
 DENP-WT-SEC            SECTION.
     MOVE     "DENP-WT"           TO        S-NAME.
***  連番カウントアップ
     ADD      1              TO   PAGE-CNT.
***  ページ指定判定
     IF  MD04 =    4
         EVALUATE  TRUE
              WHEN PGST <=   PAGE-CNT  AND  PGED >=   PAGE-CNT
                   CONTINUE
              WHEN PGST >    PAGE-CNT
                   GO   TO   DENP-WT-SEC-READ
              WHEN PGED <    PAGE-CNT
                   MOVE      "END"    TO        END-FLG
                   GO   TO   DENP-WT-EXIT
         END-EVALUATE
     END-IF.
     ADD      1              TO   IX.
***  枚数チェック
     IF       IX  >  3
**************DISPLAY "AAAAA" UPON CONS
              PERFORM NOUHIN-OUT-SEC
     END-IF.
***  ページ番号セット
     MOVE     PAGE-CNT       TO   PAGE-NO(IX).
***  発注日
     MOVE     DEN-A04(1:2)        TO   WK-HACDYMD-1.
     MOVE     "."                 TO   WK-HACDYMD-2.
     MOVE     DEN-A04(3:2)        TO   WK-HACDYMD-3.
     MOVE     "."                 TO   WK-HACDYMD-4.
     MOVE     DEN-A04(5:2)        TO   WK-HACDYMD-5.
     MOVE     WK-HACDYMD          TO   HACDYMD(IX).
***  納品日
     MOVE     DEN-A05(1:2)        TO   WK-HACDYMD-1.
     MOVE     "."                 TO   WK-HACDYMD-2.
     MOVE     DEN-A05(3:2)        TO   WK-HACDYMD-3.
     MOVE     "."                 TO   WK-HACDYMD-4.
     MOVE     DEN-A05(5:2)        TO   WK-HACDYMD-5.
     MOVE     WK-HACDYMD          TO   NOUDYMD(IX).
***  発注区分
     EVALUATE DEN-A14
         WHEN  0
         MOVE  NC"　定　期　　"   TO   HATYUK(IX)
         WHEN  1
         MOVE  NC"販促（特売）"   TO   HATYUK(IX)
         WHEN  2
         MOVE  NC"　新店改装　"   TO   HATYUK(IX)
         WHEN  3
         MOVE  NC"　投　入　　"   TO   HATYUK(IX)
         WHEN  4
         MOVE  NC"ダイレクト　"   TO   HATYUK(IX)
         WHEN  OTHER
         MOVE  NC"＊＊＊＊＊＊"   TO   HATYUK(IX)
     END-EVALUATE.
***  納品先
     MOVE     DEN-A09             TO   NOUNM(IX).
***  取引先ＣＤ
     MOVE     DEN-A061            TO   TOKCD(IX).
***  取引先名
     MOVE     DEN-A07             TO   TOKNM(IX).
***  商品名１
     MOVE     DEN-A211            TO   SYONM1(IX).
***  商品名２
     MOVE     DEN-A212            TO   SYONM2(IX).
***  伝票番号
     MOVE     DEN-A03             TO   DEN(IX).
***  行番号
     MOVE     DEN-A19             TO   GYOU(IX).
***  商品ＣＤ
     MOVE     DEN-A27             TO   WK-SYOCD.
     MOVE     WK-HEN-SYOCD        TO   JAN(IX).
***  発注数量
     MOVE     DEN-A22             TO   WK-SURYO.
     MOVE     WK-SURYO1           TO   HATYU1(IX).
     MOVE     WK-SURYO2           TO   HATYU2(IX).
***  仕入単位
***  MOVE                         TO   STAN(IX).
***  訂正数
     IF       DEN-F15  NOT =  DEN-F50
              MOVE     DEN-F15    TO   TEISEI(IX)
              MOVE     ALL "="    TO   TEISEI-GYOU(IX)
              MOVE     1          TO   TEI-FLG
     END-IF.
***  部門
     MOVE     DEN-A101            TO   BUMON(IX).
***  単価（売価単価）
     MOVE     DEN-A25             TO   BAIKA(IX).
*
 DENP-WT-SEC-READ.
     PERFORM  FL-READ-SEC.
*
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             納品シール出力                          2.4      *
****************************************************************
 NOUHIN-OUT-SEC          SECTION.
     MOVE     "NOUHIN-OUT-SEC"    TO      S-NAME.
*
*****WRITE PRT-REC FROM GYO0 AFTER 3.
     WRITE PRT-REC FROM GYO0 AFTER 2.
     WRITE PRT-REC FROM GYO1 AFTER 2.
     WRITE PRT-REC FROM GYO2 AFTER 2.
     WRITE PRT-REC FROM GYO3 AFTER 2.
     WRITE PRT-REC FROM GYO4 AFTER 1.
     WRITE PRT-REC FROM GYO5 AFTER 2.
     IF    TEI-FLG  =  1
           WRITE PRT-REC FROM GYO5-TEISEI AFTER 0
     END-IF.
     WRITE PRT-REC FROM GYO6 AFTER 2.
     MOVE  SPACE   TO    PRT-REC.
     WRITE PRT-REC AFTER PAGE.
***  各行初期化
     MOVE  SPACE   TO    GYO0.
     MOVE  SPACE   TO    GYO1.
     MOVE  SPACE   TO    GYO2.
     MOVE  SPACE   TO    GYO3.
     MOVE  SPACE   TO    GYO4.
     MOVE  SPACE   TO    GYO5.
     MOVE  SPACE   TO    GYO5-TEISEI.
     MOVE  SPACE   TO    GYO6.
     MOVE  ZERO    TO    TEI-FLG.
     MOVE  1       TO    IX.
*
 NOUHIN-OUT-EXIT.
     EXIT.
****************************************************************
*             テストプリント　                        2.4      *
****************************************************************
 TEST-PRT-SEC            SECTION.
     MOVE     "TEST-PRT"          TO      S-NAME.
*テスト印字項目セット
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 3
             MOVE   NC"Ｎ"       TO   HATYUK(IX)
             MOVE   ALL "9"      TO   DEN(IX) GYOU(IX) TOKCD(IX)
                                      JAN(IX) HATYU1(IX)
                                      HATYU1(IX) HATYU2(IX)
                                      TEISEI(IX) BUMON(IX)
                                      HACDYMD(IX) NOUDYMD(IX)
                                      BUMON(IX) BAIKA(IX)
             MOVE   ALL "*"      TO   TOKNM(IX) SYONM1(IX)
                                      SYONM2(IX)
                                      NOUNM(IX) TEISEI-GYOU(IX)
     END-PERFORM.
*明細０行目出力　　　
     WRITE    PRT-REC   FROM      GYO0      AFTER     3.
*明細１行目出力　　　
     WRITE    PRT-REC   FROM      GYO1      AFTER     2.
*明細２行目出力
     WRITE    PRT-REC   FROM      GYO2      AFTER     2.
*明細３行目出力
     WRITE    PRT-REC   FROM      GYO3      AFTER     2.
*明細４行目出力
     WRITE    PRT-REC   FROM      GYO4      AFTER     1.
*明細５行目出力
     WRITE    PRT-REC   FROM      GYO5      AFTER     2.
*明細５行目出力（訂正行）
     WRITE    PRT-REC   FROM      GYO5-TEISEI AFTER   0.
*明細６行目出力
     WRITE    PRT-REC   FROM      GYO6      AFTER     2.
*
***  各行初期化
     MOVE  SPACE   TO    GYO0.
     MOVE  SPACE   TO    GYO1.
     MOVE  SPACE   TO    GYO2.
     MOVE  SPACE   TO    GYO3.
     MOVE  SPACE   TO    GYO4.
     MOVE  SPACE   TO    GYO5.
     MOVE  SPACE   TO    GYO5-TEISEI.
     MOVE  SPACE   TO    GYO6.
     CLOSE    PRTFILE.
     OPEN     OUTPUT    PRTFILE.
*
 TEST-PRT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC          SECTION.
     MOVE     "DSP-WRITE"         TO   S-NAME.
*
     MOVE     SPACE               TO        DSP-CNTL.
     MOVE     "SCREEN"            TO        DSP-GRP.
     MOVE     "FSY39061"          TO        DSP-FMT.
*項目設定
***  伝票枚数
     MOVE     WK-MAI              TO        MD01.
***  プログラムＩＤ
     MOVE     "SSY3906L"          TO        PGID.
***  日付
     ACCEPT   SYS-DATE            FROM      DATE.
     IF       SYS-DATE(1:2)  >=   89
              MOVE      19        TO        WK-DATE8-YY1
     ELSE
              MOVE      20        TO        WK-DATE8-YY1
     END-IF.
     MOVE     SYS-DATE            TO        WK-DATE8-YY2.
     MOVE     WK-DATE8            TO        SDATE.
***  時間
     ACCEPT   SYS-TIME            FROM      TIME.
     MOVE     SYS-TIME(1:6)       TO        STIME.
***  伝票タイプ
     MOVE     "納品シールＡ"      TO        DTYPE.
***  取引先名
     MOVE     "ホーマーＧ殿向け"  TO        TORINM.
*画面出力
     WRITE    FSY39061.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面ＲＥＡＤ処理                                 *
****************************************************************
 DSP-READ-SEC           SECTION.
     MOVE    "DSP-READ"           TO   S-NAME.
*
     MOVE    "NE"                 TO        DSP-PRO.
     READ     DSPFILE.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             メッセージセット                                 *
****************************************************************
 ERR-MSG-SEC            SECTION.
     MOVE     "ERR-MSG"           TO   S-NAME.
*
     IF       ERR-FLG   =   ZERO
              MOVE   SPACE                 TO   MD05
     ELSE
              MOVE   MSG-TBL ( ERR-FLG )   TO   MD05
              MOVE   ZERO                  TO   ERR-FLG
     END-IF.
*
 ERR-MSG-EXIT.
     EXIT.
****************************************************************
*             終了処理                                3.0      *
****************************************************************
 END-SEC                SECTION.
     MOVE     "END-SEC"           TO   S-NAME.
*
     CLOSE    HMSHIRED   PRTFILE    DSPFILE.
*
 END-EXIT.
     EXIT.

```
