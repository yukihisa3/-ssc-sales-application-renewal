****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　ＨＧ基幹システム　　　　　　　　　
*    モジュール名　　　　：　エンチョー返品情報変換
*    作成日／更新日　　　：　2012/02/14
*    作成者／更新者　　　：　NAV
*    処理概要　　　　　　：　
*      受信したエンチョー返品データ（返品情報）をサカタ
*      フォーマットに変換する。
*    作成日／更新日　　　：　2023/11/21
*    作成者／更新者　　　：　NAV TAKAHASHI
*    処理概要　　　　　　：　インボイス対応
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY2720B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU-GP6000.
 OBJECT-COMPUTER.       FUJITSU-GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  受信データ（返品情報）  >>---*
     SELECT   ONLENCHE
         ASSIGN        TO  ONLENCHE
         ORGANIZATION  IS  SEQUENTIAL
         ACCESS MODE   IS  SEQUENTIAL
         FILE  STATUS  IS  OEH-STS.

*---<<  エンチョー返品情報データ  >>---*
     SELECT  ECHENPF
         ASSIGN        TO  VS-ECHENPF *> PF
         ORGANIZATION  IS  SEQUENTIAL
         ACCESS MODE   IS  SEQUENTIAL
         FILE STATUS   IS  ECH-STS.

 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  受信データ（返品情報）  >>---*
 FD  ONLENCHE
     BLOCK CONTAINS 1 RECORDS
     LABEL RECORD   IS STANDARD.
     COPY  ONLENCHH OF XFDLIB   JOINING  OEH-H  PREFIX.
     COPY  ONLENCHD OF XFDLIB   JOINING  OEH-D  PREFIX.
     COPY  ONLENCHM OF XFDLIB   JOINING  OEH-M  PREFIX.

*---<<  エンチョー返品情報データ  >>---*
 FD  ECHENPF.
     COPY  ECHENPF OF XFDLIB   JOINING  ECH  PREFIX.

****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     03  OEH-STS            PIC  X(02).
     03  ECH-STS            PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     03  END-FLG            PIC  X(03)  VALUE SPACE.
     03  FG-ONLENCHE-END    PIC  X(03)  VALUE SPACE.
     03  FG-ECHENPF-INV     PIC  9(01).

 01  CNT-AREA.
     03  CT-IN              PIC  9(07)  VALUE ZERO.
     03  CT-DEN-MAISU       PIC  9(06)  VALUE ZERO.
     03  CT-MS-GYOSU      PIC  9(06)  VALUE ZERO.

****  ＷＲＫ領域  ***
 01  WRK-AREA.
     03  WRK-DATE1          PIC  9(06).
     03  WRK-DATE1R  REDEFINES WRK-DATE1.
         05  WRK-DATE1R1    PIC  9(04).
         05  WRK-DATE1R2    PIC  9(02).
     03  WRK-DATE2          PIC  9(06).

** 返品情報（エンチョー->ベンダー)
  *> 取引先ヘッダーレコード
     COPY  ONLENCHH OF XFDLIB   JOINING  OEH-HW  PREFIX.
  *> 返品伝票ヘッダーレコード
     COPY  ONLENCHD OF XFDLIB   JOINING  OEH-DW  PREFIX.

**** メッセージ情報  ***
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER           PIC  X(04)  VALUE  "### ".
       05  ERR-PG-ID        PIC  X(08)  VALUE  "SSY2720B".
       05  FILLER           PIC  X(10)  VALUE  " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER           PIC  X(04)  VALUE  "### ".
       05  ERR-FL-ID        PIC  X(08).
       05  FILLER           PIC  X(04)  VALUE  " ST-".
       05  ERR-STCD         PIC  X(02).
       05  FILLER           PIC  X(04)  VALUE  " ###".
* 日付
 01  DATE-AREA.
     03  WK-YS              PIC  9(02)  VALUE ZERO.
     03  WK-DATE.
         05  WK-Y           PIC  9(02)  VALUE ZERO.
         05  WK-M           PIC  9(02)  VALUE ZERO.
         05  WK-D           PIC  9(02)  VALUE ZERO.
 01  DATE-AREAR1  REDEFINES DATE-AREA.
     03  SYS-YM             PIC  9(06).
     03  FILLER             PIC  9(02).
 01  DATE-AREAR2  REDEFINES DATE-AREA.
     03  SYS-DATE           PIC  9(08).

* 時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE ZERO.
 01  TIME-AREAR  REDEFINES TIME-AREA.
     03  WK-TIME-HMS        PIC  9(06).
     03  WK-TIME-MS         PIC  9(06).

*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN            PIC  X(01).
 01  LINK-IN-YMD6           PIC  9(06).
 01  LINK-IN-YMD8           PIC  9(08).
 01  LINK-OUT-RET           PIC  X(01).
 01  LINK-OUT-YMD           PIC  9(08).

 01  MSG-AREA.
     03  MSG-010.
       05  MSG-010-1.
           07  FILLER  PIC  X(32) VALUE
           "＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
       05  MSG-010-2.
           07  FILLER  PIC  X(22) VALUE
           "＊　返品情報READ＝".
           07  MSG-010-2-IN           PIC  9(06).
           07  FILLER  PIC  X(08) VALUE
           "件　＊".
       05  MSG-010-3.
           07  FILLER  PIC  X(20) VALUE
           "＊　返品伝票枚数＝".
           07  MSG-010-3-DEN-MAISU    PIC  9(06).
           07  FILLER  PIC  X(08) VALUE
           "件　＊".
       05  MSG-010-4.
           07  FILLER  PIC  X(20) VALUE
           "＊　返品明細行数＝".
           07  MSG-010-4-MS-GYOSU     PIC  9(06).
           07  FILLER  PIC  X(08) VALUE
           "件　＊".

*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.

 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER  EXCEPTION
                PROCEDURE  ONLENCHE.
     MOVE  "ONLENCHE"     TO  ERR-FL-ID.
     MOVE  OEH-STS        TO  ERR-STCD.
     DISPLAY  MSG-ABEND1  UPON CONS.
     DISPLAY  MSG-ABEND2  UPON CONS.
     MOVE  4000           TO  PROGRAM-STATUS.
     STOP RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER  EXCEPTION
                PROCEDURE  ECHENPF.
     MOVE  "ECHENPF"      TO  ERR-FL-ID.
     MOVE  ECH-STS        TO  ERR-STCD.
     DISPLAY  MSG-ABEND1  UPON CONS.
     DISPLAY  MSG-ABEND2  UPON CONS.
     MOVE  4000           TO  PROGRAM-STATUS.
     STOP RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP  RUN.
 KEI0100-EXIT.
     EXIT.
****************************************************************
*    1.0  初期処理                                             *
****************************************************************
 INIT-SEC                   SECTION.
*ファイルのＯＰＥＮ
     DISPLAY   "**  START SSY2720B  **"  UPON CONS.
     OPEN  INPUT  ONLENCHE.
     OPEN  OUTPUT ECHENPF.

*システム日付・時刻の取得
     ACCEPT  WK-DATE  FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING  LINK-IN-KBN
                              LINK-IN-YMD6
                              LINK-IN-YMD8
                              LINK-OUT-RET
                              LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  DATE-AREA.

*システム日付取得
     ACCEPT  WK-TIME   FROM  TIME.

     PERFORM  INF-READ-SEC.
     IF  FG-ONLENCHE-END = "END"
         MOVE  "END"        TO  END-FLG
         GO TO  INIT-EXIT
     END-IF.

     IF  OEH-H-F02 NOT = "A"
         DISPLAY "SSY2720B HERDER RECORD NOT FOUND ???"
                 UPON CONS
         MOVE  4001         TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-IF.

 INIT-EXIT.
     EXIT.
****************************************************************
*    入力ファイル読込み                                        *
****************************************************************
 INF-READ-SEC               SECTION.

     READ  ONLENCHE
       AT END
         MOVE  "END"        TO  FG-ONLENCHE-END
         GO TO  INF-READ-EXIT
     END-READ.

     ADD 1  TO CT-IN.

*****DISPLAY "REC = " OEH-H-F03 UPON CONS.
*****DISPLAY "OEH-H-F03 = " OEH-H-F03  UPON CONS.
     IF  OEH-H-F03  NOT  NUMERIC
         GO TO  INF-READ-SEC
     END-IF.

 INF-READ-EXIT.
     EXIT.
****************************************************************
*    2.0  メイン処理                                           *
****************************************************************
 MAIN-SEC                   SECTION.

     MOVE  OEH-H-REC        TO  OEH-HW-REC.
     PERFORM  INF-READ-SEC.

     PERFORM  UNTIL FG-ONLENCHE-END = "END"
                 OR OEH-H-F02       = "A"
                                   *> 取引先ヘッダブレイク
       PERFORM  EDWT-SEC

     END-PERFORM.

     IF  FG-ONLENCHE-END = "END"
         MOVE  "END"        TO  END-FLG
     END-IF.

 MAIN-EXIT.
     EXIT.
****************************************************************
*    編集／出力                                                *
****************************************************************
 EDWT-SEC                   SECTION.
     IF  OEH-H-F02 = "B"
         MOVE  OEH-D-REC    TO  OEH-DW-REC
         PERFORM  INF-READ-SEC
         ADD  1   TO  CT-DEN-MAISU

     END-IF.

     PERFORM  UNTIL FG-ONLENCHE-END = "END"
                 OR OEH-H-F02       = "A"
                 OR OEH-H-F02       = "B"
                                   *> 取引先ヘッダブレイク
                                   *> 返品伝票ブレイク
       PERFORM  EDWTB-SEC
       PERFORM  INF-READ-SEC

     END-PERFORM.

 EDWT-EXIT.
     EXIT.
****************************************************************
*    データ部編集／出力Ｂ                                      *
****************************************************************
 EDWTB-SEC                  SECTION.
      MOVE  SPACE           TO  ECH-REC.
      INITIALIZE  ECH-REC.

      MOVE  OEH-HW-F01      TO  ECH-F011. *> データ区分
      MOVE  OEH-HW-F02      TO  ECH-F012. *> レコード区分
      MOVE  OEH-HW-F03      TO  ECH-F013. *> ﾚｺｰﾄﾞｼｰｹﾝｽNO
      MOVE  OEH-HW-F04      TO  ECH-F014. *> 取引先ＣＤ
      MOVE  OEH-HW-F05      TO  ECH-F015. *> データ送信元
      MOVE  OEH-HW-F06      TO  ECH-F016. *> データ作成日
      MOVE  OEH-HW-F07      TO  ECH-F017. *> データ作成時刻
      MOVE  OEH-HW-F08      TO  ECH-F018. *> 送信レコード件数
      MOVE  OEH-HW-F09      TO  ECH-F019. *> 取引先名称

      MOVE  OEH-DW-F01      TO  ECH-F021. *> データ区分
      MOVE  OEH-DW-F02      TO  ECH-F022. *> レコード区分
      MOVE  OEH-DW-F03      TO  ECH-F023. *> ﾚｺｰﾄﾞｼｰｹﾝｽNO
      MOVE  OEH-DW-F04      TO  ECH-F024. *> 受入先（店）ＣＤ
      MOVE  OEH-DW-F05      TO  ECH-F025. *> 受入先（課）ＣＤ
      MOVE  OEH-DW-F06      TO  ECH-F026. *> 返品伝票Ｎｏ
      MOVE  OEH-DW-F07      TO  ECH-F027. *> 返品明細管理番号
      MOVE  OEH-DW-F08      TO  ECH-F028. *> 伝票内訳
      MOVE  OEH-DW-F09      TO  ECH-F029. *> 部門ＣＤ
      MOVE  OEH-DW-F10      TO  ECH-F02A. *> 大分類ＣＤ
      MOVE  OEH-DW-F11      TO  ECH-F02B. *> 処理日
      MOVE  OEH-DW-F12      TO  ECH-F02C. *> 支払対象締日

      IF  OEH-DW-F13 = "-"
          COMPUTE ECH-F02D  =  OEH-DW-F14 * -1  *> 原価合計額
      ELSE
          MOVE  OEH-DW-F14  TO  ECH-F02D *> 原価合計額
      END-IF.

      MOVE  OEH-DW-F15      TO  ECH-F02E. *> 課税区分

      MOVE  OEH-M-F01       TO  ECH-F031. *> データ区分
      MOVE  OEH-M-F02       TO  ECH-F032. *> レコード区分
      MOVE  OEH-M-F03       TO  ECH-F033. *> ﾚｺｰﾄﾞｼｰｹﾝｽNO
      MOVE  OEH-M-F04       TO  ECH-F034. *> 伝票行Ｎｏ
      MOVE  OEH-M-F05       TO  ECH-F035. *> 返品明細書行Ｎｏ
      MOVE  OEH-M-F06       TO  ECH-F036. *> 商品ＪＡＮＣＤ
      MOVE  OEH-M-F07       TO  ECH-F037. *> 商品ＥＯＳＣＤ
      MOVE  OEH-M-F08       TO  ECH-F038. *> 品名
      MOVE  OEH-M-F09       TO  ECH-F039. *> 規格

      IF  OEH-M-F10 = "-"
          COMPUTE ECH-F03A  =  OEH-M-F11 * -1  *> 数量
      ELSE
          MOVE  OEH-M-F11   TO  ECH-F03A *> 数量
      END-IF.

      IF  OEH-M-F12 = "-"
          *> 入力定義 9(09) -> データ内容 9(07)V9(02)

          COMPUTE ECH-F03B  =
              ( OEH-M-F13 / 100 ) * -1  *> 原単価
      ELSE
          COMPUTE ECH-F03B  =  OEH-M-F13 / 100  *> 原単価
      END-IF.

      MOVE  OEH-M-F14       TO  ECH-F03C. *> 返品理由
*#2023/11/21 NAV ST OEH-DW
      MOVE  OEH-DW-F16      TO  ECH-F04.  *> 税率
      MOVE  OEH-DW-F17      TO  ECH-F05.  *> 譲渡年月１
      MOVE  OEH-DW-F18      TO  ECH-F06.  *> 譲渡年月２
*#2023/11/21 NAV ED

      MOVE  SYS-DATE        TO  ECH-F98.  *> 作成日付
      MOVE  WK-TIME-HMS     TO  ECH-F99.  *> 作成時刻

      WRITE  ECH-REC.

      ADD  1   TO  CT-MS-GYOSU.

 EDWTB-EXIT.
     EXIT.
****************************************************************
*    3.0  終了処理                                             *
****************************************************************
 END-SEC                    SECTION.
     CLOSE  ONLENCHE.
     CLOSE  ECHENPF.

     MOVE  CT-IN            TO  MSG-010-2-IN.
     MOVE  CT-DEN-MAISU     TO  MSG-010-3-DEN-MAISU.
     MOVE  CT-MS-GYOSU      TO  MSG-010-4-MS-GYOSU.
     DISPLAY  MSG-010-1  UPON CONS.
     DISPLAY  MSG-010-2  UPON CONS.
     DISPLAY  MSG-010-3  UPON CONS.
     DISPLAY  MSG-010-4  UPON CONS.
     DISPLAY  MSG-010-1  UPON CONS.
     DISPLAY   "**  END   SSY2720B  **"  UPON CONS.

 END-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************
