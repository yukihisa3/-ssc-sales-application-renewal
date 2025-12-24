****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ダイユーエイトオンライン　        *
*    モジュール名　　　　：　ＴＣ納品（納品シール発行）        *
*    作成日／更新日　　　：　05/08/02                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    伝票フォーマット　　：　専用用紙                          *
*                            摘要欄に訂正後数量を追加 2005/09/26
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY7302L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/08/24.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA             IS        YA
         YA-21          IS        YA-21
         YA-22          IS        YA-22
         CONSOLE        IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ダイユーエイト伝票データ（共通）
     SELECT   DYSHIRED  ASSIGN    TO        DYSHIRED
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
*  FILE =ダイユーエイト伝票データ（共通）                      *
****************************************************************
 FD  DYSHIRED
                        BLOCK    CONTAINS  1    RECORDS
                        LABEL    RECORD    IS   STANDARD.
                        COPY     DYSHIRED  OF   XFDLIB
                        JOINING  DEN       AS   PREFIX.
****************************************************************
*    FILE = ｶﾞﾒﾝ  F                                            *
****************************************************************
 FD  DSPFILE
                        LABEL RECORD   IS   OMITTED.
*
     COPY    FSY38041   OF   XMDLIB.
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
***  ｶｳﾝﾄ
 01  CNT-AREA.
     03  L-CNT                    PIC  9(07)     VALUE  ZERO.
     03  CNT-AFTER                PIC  9(07)     VALUE  ZERO.
     03  PAGE-CNT                 PIC  9(04)     VALUE  ZERO.
     03  IX                       PIC  9(02)     VALUE  ZERO.
     03  IY                       PIC  9(02)     VALUE  ZERO.
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
 01  WK-SYOCD                     PIC  X(13).
 01  WK-SYOCD-R  REDEFINES  WK-SYOCD.
     03  WK-HEN-SYOCD             PIC  9(13).
***  画面用日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1             PIC  9(02).
     03  WK-DATE8-YY2             PIC  9(06).
***  出力取引先ＣＤ
 01  WK-TOKCD                     PIC  9(08)   VALUE  1225.
 01  WK-KANJI.
     03  KANJI                    PIC  N(10)  VALUE
         NC"１２３４５６７８９０".
 01  WK-KANJIR                    REDEFINES   WK-KANJI.
     03  WK-SU                    OCCURS      10.
         05  SU                   PIC  N(01).
 01  WK-DENNO                     PIC  9(08)  VALUE  ZERO.
 01  WK-DENNO-N                   PIC  N(08)  VALUE  SPACE.
*原単価編集用ﾜｰｸ
 01  WK-GENKA                     PIC  9(07)V99.
 01  WK-GENKA-R  REDEFINES  WK-GENKA.
     03  WK-GENKA1                PIC  9(07).
     03  WK-GENKA2                PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
*０行目
 01  GYO0                         CHARACTER  TYPE  IS  YA-22.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
***  区分
     03  HEADER                   PIC  N.
     03  FILLER                   PIC  X(32)     VALUE   SPACE.
***  ページ番号
     03  PAGENO                   PIC  ZZ9.
*一行目
 01  GYO1.
     03  FILLER                   PIC  X(48)     VALUE   SPACE.
*    取引先名(右)
     03  TORIN2                   PIC  X(22).
*二行目
 01  GYO2.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    注文日（年）
     03  TYUMNY                   PIC  9(02).
     03  SLASH1                   PIC  X(01).
*    注文日（月）
     03  TYUMNM                   PIC  Z9.
     03  SLASH2                   PIC  X(01).
*    注文日（日）
     03  TYUMND                   PIC  Z9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    注文番号（左）
     03  TYUNO1                   PIC  9(08).
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
*    納品指定日(年)
     03  NOUSTY                   PIC  99.
     03  SLASH3                   PIC  X(01).
*    納品指定日（月）
     03  NOUSTM                   PIC  Z9.
     03  SLASH4                   PIC  X(01).
*    納品指定日（日）
     03  NOUSTD                   PIC  Z9.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
*    部門（左）
     03  BUMON1                   PIC  9(02).
*
*三行目
 01  GYO3                         CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
*    取引先コード（左）
     03  TORIC1                   PIC  9(04).
     03  FILLER                   PIC  X(03)     VALUE   SPACE.
*    取引先名（左）
     03  TORIN1                   PIC  X(20).
     03  FILLER                   PIC  X(20)     VALUE   SPACE.
*    区分
     03  KUBUNN                   PIC  N(02).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    注文番号（右）
     03  TYUNO2                   PIC  N(08).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    取引先コード（右）
     03  TORIC2                   PIC  9(04).
*
*四行目
 01  GYO4.
     03  FILLER                   PIC  X(59)     VALUE   SPACE.
*    部門（右）
     03  BUMON2                   PIC  9(02).
*
*五行目
 01  GYO5.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    商品コード（左上）
     03  SYCD11                   PIC  X(07).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    商品名（左上）
     03  SYNM11                   PIC  X(15).
     03  FILLER                   PIC  X(24)     VALUE   SPACE.
*    商品名（右上）
     03  SYNM21                   PIC  X(15).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    商品コード（右）
     03  SYCD21                   PIC  X(13).
*
*六行目
 01  GYO6.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    商品コード（左下）
     03  SYCD12                   PIC  X(07).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    商品名（左下）
     03  SYNM12                   PIC  X(15).
     03  FILLER                   PIC  X(24)     VALUE   SPACE.
*    商品名（右下）
     03  SYNM22                   PIC  X(15).
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
*    売価(右)
     03  BAIKA2                   PIC  Z,ZZZ,ZZ9.
*
*七行目
 01  GYO7.
     03  FILLER                   PIC  X(04)     VALUE   SPACE.
*    数量(左)
     03  SURYO1                   PIC  ZZZZZ9.
*    単価
     03  TANKA1                   PIC  ZZZZZZ9.
     03  TANKA2                   PIC  99.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    金額
     03  KINGAK                   PIC  ZZZZZZZZ9.
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
*    売価（左）
     03  BAIKA1                   PIC  Z,ZZZ,ZZ9.
*
*八行目
 01  GYO8.
     03  FILLER                   PIC  X(50)     VALUE   SPACE.
*    数量（右）
     03  SURYO2                   PIC  ZZZZZ9.
*
*数量訂正用
 01  HTEISEI.
     03  FILLER                   PIC  X(01)       VALUE   SPACE.
     03  HTSEN                    PIC  X(09)  VALUE  "=========".
 01  TEISEI                       CHARACTER TYPE   IS      YA.
*****03  FILLER                   PIC  X(50)       VALUE   SPACE.
     03  FILLER                   PIC  X(18)       VALUE   SPACE.
*****03  TEK-TSSUR1               PIC  N(03)       VALUE   SPACE.
*****03  TEK-TSSUR2               PIC  X(01)       VALUE   SPACE.
*****03  TEK-TSSURYO              PIC  ZZZZZ9.
     03  TEK-TSSUR1               PIC  N(03).
     03  TEK-TSSUR2               PIC  X(01).
     03  TEK-TSSURYO              PIC  ZZZZZ9.
     03  FILLER                   PIC  X(19)       VALUE   SPACE.
     03  TSSEN                    PIC  X(06).
*    訂正後数量
     03  TSSURY                   PIC  ZZZZZ9.
*
*_番
 01  TANABAN                      CHARACTER TYPE IS      YA.
     03  FILLER                   PIC  X(05)     VALUE   SPACE.
     03  TEK-TANNM1               PIC  N(02)     VALUE   SPACE.
     03  TEK-TANNM2               PIC  X(01)     VALUE   SPACE.
     03  TEK-TANABAN              PIC  X(06).

*九行目
 01  GYO9.
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    摘要１（左）
     03  TKYO11                   PIC  X(12).
     03  FILLER                   PIC  X(01)     VALUE   SPACE.
*    摘要２（左）
     03  TKYO12                   PIC  X(12).
     03  FILLER                   PIC  X(28)     VALUE   SPACE.
*    摘要１（右）
     03  TKYO21                   PIC  X(12).
     03  FILLER                   PIC  X(02)     VALUE   SPACE.
*    摘要２（右）
     03  TKYO22                   PIC  X(12).
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
     USE AFTER          EXCEPTION      PROCEDURE      DYSHIRED.
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
     OPEN     INPUT     DYSHIRED.
     OPEN     I-O       DSPFILE.
     OPEN     OUTPUT    PRTFILE.
*    初期値セット
     MOVE     ZERO                TO   DENNO1.
     MOVE     ZERO                TO   CNT-DENPYO.
     MOVE     SPACE               TO   END-FLG.
     MOVE     SPACE               TO   FSY38041.
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
     CLOSE    DYSHIRED.
*
     OPEN     INPUT      DYSHIRED.
 INIT-EXIT.
     EXIT.
****************************************************************
*             伝票データファイル読込み                2.0      *
****************************************************************
 DEN-CNT-SEC            SECTION.
*
     READ     DYSHIRED  AT  END
              MOVE     "END"     TO   END-FLG
              GO                 TO   DEN-CNT-EXIT
     END-READ.
*    ダイユーエイト以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  WK-TOKCD
              GO                 TO   DEN-CNT-SEC
     END-IF.
*    納品区分＝”2”（センター）以外読み飛ばし
     IF       DEN-A18  NOT = "2"
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
     MOVE          SPACE               TO   FSY38041.
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
                   CLOSE     DYSHIRED  PRTFILE
                   OPEN      INPUT     DYSHIRED
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
                   CLOSE     DYSHIRED  PRTFILE
                   OPEN      INPUT     DYSHIRED
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
     READ     DYSHIRED    AT      END
              MOVE       "END"    TO        END-FLG
              MOVE        1       TO        ERR-FLG
              PERFORM   ERR-MSG-SEC
              GO                  TO        FL-READ-EXIT
     END-READ.
*行■０の時
     IF       DEN-A03 =    ZERO
              GO                  TO        READ-000
     END-IF.
*    ダイユーエイト以外は読み飛ばし（取引先）
     IF       DEN-F01  NOT =  WK-TOKCD
              GO                 TO   FL-READ-SEC
     END-IF.
*    プリント区分＝”2”（納品カード）以外読み飛ばし
     IF       DEN-A18  NOT =  "2"
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
***  初期化
     MOVE     SPACE               TO   GYO0.
     MOVE     SPACE               TO   GYO1.
     MOVE     SPACE               TO   GYO2.
     MOVE     SPACE               TO   GYO3.
     MOVE     SPACE               TO   GYO4.
     MOVE     SPACE               TO   GYO5.
     MOVE     SPACE               TO   GYO6.
     MOVE     SPACE               TO   GYO7.
     MOVE     SPACE               TO   GYO8.
     MOVE     SPACE               TO   TEISEI.
     MOVE     SPACE               TO   TANABAN.
     MOVE     SPACE               TO   GYO9.
***  連番カウントアップ
     ADD      1                   TO   PAGE-CNT.
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
***  区分（欄外）
     IF  DEN-A16 = 1
              MOVE     NC"Ｆ"   TO   HEADER
     ELSE
              MOVE     NC"Ｓ"   TO   HEADER
     END-IF.
***  ページ番号
     MOVE     PAGE-CNT            TO   PAGENO.
***  取引先名
     MOVE     DEN-A13             TO   TORIN1 TORIN2.
***  注文日
     MOVE     DEN-A08(1:2)        TO   TYUMNY.
     MOVE     DEN-A08(3:2)        TO   TYUMNM.
     MOVE     DEN-A08(5:2)        TO   TYUMND.
***  注文番号(伝票番号)
     MOVE     DEN-A03             TO   TYUNO1.
*****MOVE     DEN-A03             TO   TYUNO2.
     MOVE     DEN-A03             TO   WK-DENNO.
     MOVE     SPACE               TO   WK-DENNO-N.
     PERFORM  VARYING  IX  FROM 1 BY 1 UNTIL IX > 8
              IF  WK-DENNO(IX:1)  =  ZERO
                  MOVE   SU(10)     TO  WK-DENNO-N(IX:1)
              ELSE
                  MOVE WK-DENNO(IX:1) TO  IY
                  MOVE SU(IY)       TO  WK-DENNO-N(IX:1)
              END-IF
     END-PERFORM.
     MOVE     WK-DENNO-N          TO   TYUNO2.
***  納品指定日
     MOVE     DEN-A09(1:2)        TO   NOUSTY.
     MOVE     DEN-A09(3:2)        TO   NOUSTM.
     MOVE     DEN-A09(5:2)        TO   NOUSTD.
     MOVE     "/"                 TO   SLASH1  SLASH2
                                       SLASH3  SLASH4.
***  部門
     MOVE     DEN-A06(3:2)        TO   BUMON1 BUMON2.
***  取引先コード
     MOVE     DEN-A10             TO   TORIC1 TORIC2.
***  区分
     IF  DEN-A16 = 1
              MOVE     NC"Ｆ　"   TO   KUBUNN
     ELSE
              MOVE     NC"Ｓ　"   TO   KUBUNN
     END-IF.
***  商品コード(上)
     MOVE     DEN-A24(1:7)        TO   SYCD11.
***  商品コード(下)
     MOVE     DEN-A24(8:6)        TO   SYCD12.
***  商品コード
     MOVE     DEN-A24             TO   SYCD21.
***  商品名(上)
     MOVE     DEN-A34(1:15)       TO   SYNM11 SYNM21.
***  商品名(下)
     MOVE     DEN-A34(16:15)      TO   SYNM12 SYNM22.
***  売価
     MOVE     DEN-A30             TO   BAIKA1 BAIKA2.
***  数量
     MOVE     DEN-F50             TO   SURYO1 SURYO2.
***  単価（原価単価）
     MOVE     DEN-A29             TO   WK-GENKA.
     MOVE     WK-GENKA1           TO   TANKA1.
     MOVE     WK-GENKA2           TO   TANKA2.
***  金額
     MOVE     DEN-A31             TO   KINGAK.
***  訂正後数量
     IF       DEN-F15  NOT =  DEN-F50
              MOVE     "======"   TO   TSSEN
              MOVE     DEN-F15    TO   TSSURY
**** ELSE
****          MOVE     ZERO       TO   TSSURY
     END-IF.
*    訂正後数量（控え）
*****IF       DEN-F50  NOT  =     DEN-F15
*********MOVE     NC"訂正後"          TO   TEK-TSSUR1
*********MOVE     ":"                 TO   TEK-TSSUR2
*********MOVE     DEN-F15             TO   TEK-TSSURYO
*****END-IF.
*    _番
     MOVE     NC"_番"            TO   TEK-TANNM1.
     MOVE     ":"                 TO   TEK-TANNM2.
     MOVE     DEN-F49             TO   TEK-TANABAN.

***  摘要１
     MOVE     DEN-A14             TO   TKYO11 TKYO21.
***  摘要２
     MOVE     DEN-A15             TO   TKYO12 TKYO22.
*明細０行目出力　　　
     WRITE    PRT-REC   FROM      GYO0      AFTER     2.
*明細１行目出力　　　
     WRITE    PRT-REC   FROM      GYO1      AFTER     2.
*明細２行目出力
     WRITE    PRT-REC   FROM      GYO2      AFTER     1.
*明細３行目出力
     WRITE    PRT-REC   FROM      GYO3      AFTER     2.
*明細４行目出力
     WRITE    PRT-REC   FROM      GYO4      AFTER     1.
*明細５行目出力
     WRITE    PRT-REC   FROM      GYO5      AFTER     1.
*明細６行目出力
     WRITE    PRT-REC   FROM      GYO6      AFTER     1.
*明細７行目出力　　　
     WRITE    PRT-REC   FROM      GYO7      AFTER     2.
***  数量訂正ありの場合
     IF       DEN-F15   NOT =  DEN-F50
              WRITE    PRT-REC  FROM  HTEISEI AFTER   0
     END-IF.
*明細８行目出力　　　
     WRITE    PRT-REC   FROM      TANABAN   AFTER     1.
     WRITE    PRT-REC   FROM      GYO8      AFTER     0.
***  数量訂正ありの場合
     IF       DEN-F15   NOT =  DEN-F50
         MOVE     NC"訂正後"          TO   TEK-TSSUR1
         MOVE     ":"                 TO   TEK-TSSUR2
         MOVE     DEN-F15             TO   TEK-TSSURYO
              WRITE    PRT-REC  FROM  TEISEI  AFTER   0
     END-IF.
*明細９行目出力　　　
     WRITE    PRT-REC   FROM      GYO9      AFTER     1.
*    改ページ
     MOVE     SPACE     TO        PRT-REC.
     WRITE    PRT-REC   AFTER     PAGE.
*
 DENP-WT-SEC-READ.
     PERFORM  FL-READ-SEC.
*
 DENP-WT-EXIT.
     EXIT.
****************************************************************
*             テストプリント　                        2.4      *
****************************************************************
 TEST-PRT-SEC            SECTION.
     MOVE     "TEST-PRT"          TO      S-NAME.
*テスト印字項目セット
     MOVE   NC"Ｎ"               TO   HEADER.
     MOVE   NC"ＮＮ"             TO   KUBUNN.
     MOVE   NC"ＮＮＮＮＮＮＮＮ"
                                 TO   TYUNO2.
     MOVE   ALL "9"              TO   TYUMNY TYUMNM TYUMND
                                      TYUNO1
                                      NOUSTY NOUSTM NOUSTD
                                      BUMON1 BUMON2
                                      TORIC1 TORIC2
                                      BAIKA1 BAIKA2
                                      TANKA1 TANKA2
                                      SURYO1 SURYO2 KINGAK
                                      TSSURY.
     MOVE   ALL "*"              TO   TORIN1 TORIN2
                                      SYNM11 SYNM12
                                      SYNM21 SYNM22
                                      TKYO11 TKYO12
                                      TKYO21 TKYO22.
*明細０行目出力　　　
     WRITE    PRT-REC   FROM      GYO0      AFTER     2.
*明細１行目出力　　　
     WRITE    PRT-REC   FROM      GYO1      AFTER     2.
*明細２行目出力
     WRITE    PRT-REC   FROM      GYO2      AFTER     1.
*明細３行目出力
     WRITE    PRT-REC   FROM      GYO3      AFTER     2.
*明細４行目出力
     WRITE    PRT-REC   FROM      GYO4      AFTER     1.
*明細５行目出力
     WRITE    PRT-REC   FROM      GYO5      AFTER     1.
*明細６行目出力
     WRITE    PRT-REC   FROM      GYO6      AFTER     1.
*明細７行目出力　　　
     WRITE    PRT-REC   FROM      GYO7      AFTER     2.
*明細８行目出力　　　
     WRITE    PRT-REC   FROM      GYO8      AFTER     1.
*明細９行目出力　　　
     WRITE    PRT-REC   FROM      GYO9      AFTER     1.
*
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
     MOVE     "FSY38041"          TO        DSP-FMT.
*項目設定
***  伝票枚数
     MOVE     WK-MAI              TO        MD01.
***  プログラムＩＤ
     MOVE     "SSY7302L"          TO        PGID.
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
     MOVE     "納品カード"    TO        DTYPE.
***  取引先名
     MOVE     "ダイユーエイト殿"  TO        TORINM.
*画面出力
     WRITE    FSY38041.
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
     CLOSE    DYSHIRED   PRTFILE    DSPFILE.
*
 END-EXIT.
     EXIT.
