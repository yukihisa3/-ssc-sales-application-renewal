# SSI3701B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI3701B.COB`

## ソースコード

```cobol
***********************************************************
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：支払明細データ取込＆変換（ＣＤ）   *
*    作成日／更新日　：2012/04/16                         *
*    作成者／更新者　：ＮＡＶ　　                         *
*    処理概要　　　　：                                   *
*      支払明細データを支払明細ファイルに変換し累積する。 *
*    作成日／更新日　：2012/10/19                         *
*      ＩＮＰＵＴファイルを変更　　　　　　　　　　　　　 *
*    作成日／更新日　：2012/11/08                         *
*      返品・値引伝票の場合、金額に－１を掛ける　　　　　 *
*    作成日／更新日　：2013/03/21                         *
*      伝票番号の属性を、数値→文字に変更　　　　　　　　 *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSI3701B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2012/04/16.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*支払明細データ
     SELECT  SIHANAF
       ASSIGN        TO  DA-01-S-SIHANAF
       FILE STATUS   IS  NSI-ST.

*受領累積ファイル
     SELECT  SITGKF90
       ASSIGN        TO  DA-01-S-SITGKF90
       FILE  STATUS  IS  SIH-ST.
*
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 支払明細データ                                     *
****************************************************************
 FD  SIHANAF          BLOCK     CONTAINS   6   RECORDS
                      LABEL     RECORD    IS   STANDARD.
                      COPY      SIHANAF   OF   XFDLIB
                      JOINING   NSI       AS   PREFIX.
*01  NSI-REC.
*    03  NSI-F01                PIC  X(01).
*    03  FILLER                 PIC  X(79).
*
******************************************************************
*    支払明細ファイル
******************************************************************
 FD  SITGKF90         BLOCK     CONTAINS  20   RECORDS
                      LABEL     RECORD    IS   STANDARD.
                      COPY      SITGKF90  OF   XFDLIB
                      JOINING   SIH       AS   PREFIX.

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  NSI-ST             PIC  X(02).
     03  SIH-ST             PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  NSI-ERR            PIC N(15) VALUE
         NC"ナフコ支払データＣＤエラー".
     03  SIH-ERR            PIC N(15) VALUE
         NC"支払明細ファイルエラー".
*読込フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  IN-CNT             PIC  9(07)  VALUE ZERO.
     03  OT-CNT             PIC  9(07)  VALUE ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME             PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE             PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER             PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST               PIC  9(02).
*日付変換サブルーチン用ワーク
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
 01  WK-AREA.
     03  WK-KINGAKU               PIC S9(09)  VALUE  ZERO.
     03  WK-NSI-F23               PIC  9(09)  VALUE  ZERO.
*
*支払明細編集レコード定義（ヘッダ）
*01  WSH-REC.
*    03  WSH-F01                PIC  X(40).
*    03  WSH-F02                PIC  9(08).
*    03  WSH-F03                PIC  9(08).
*    03  WSH-F04                PIC  X(24).
*支払明細編集レコード定義（明細）
*01  WSI-REC.
*    03  WSI-F01                PIC  X(01).
*    03  WSI-F02                PIC  X(02).
*    03  WSI-F03                PIC  9(06).
*    03  WSI-F04                PIC  9(05).
*    03  WSI-F05                PIC  9(08).
*    03  WSI-F06                PIC  9(06).
*    03  WSI-F07                PIC  9(03).
*    03  WSI-F08                PIC  9(02).
*    03  WSI-F09                PIC  9(11).
*    03  WSI-F10                PIC  X(01).
*    03  WSI-F11                PIC  9(09).
*    03  WSI-F12                PIC  X(01).
*    03  WSI-F13                PIC  9(09).
*    03  WSI-F14                PIC  9(02).
*    03  WSI-F15                PIC  9(02).
*    03  WSI-F16                PIC  X(12).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 SHD-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SIHANAF.
     MOVE        NSI-ST     TO       E-ST.
     MOVE        "SIHANAF" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NSI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SIH-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SITGKF90.
     MOVE        SIH-ST    TO        E-ST.
     MOVE        "SITGKF90" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SIH-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
     STOP RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.
* ファイルのＯＰＥＮ
     OPEN  INPUT  SIHANAF.
     OPEN  OUTPUT SITGKF90.
* システム日付取得
     ACCEPT  WK-DATE  FROM DATE.
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  WK-DATE          TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD  TO  SYS-DATE.

     PERFORM  RD-INF-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*  入力ファイル読込み処理                            1.1       *
****************************************************************
 RD-INF-SEC              SECTION.
*
     READ  SIHANAF
       AT  END
         MOVE  "END"        TO  END-FLG
         GO TO  RD-INF-EXIT
     END-READ.
*
     ADD  1   TO  IN-CNT.
*
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "RD-CNT = " IN-CNT UPON CONS
     END-IF.
*
*----IF  NSI-F01  NOT =  "B"
*----    IF  NSI-F01  =  "H"
*----        MOVE  NSI-REC   TO  WSH-REC
*----    END-IF
*----    GO TO  RD-INF-SEC
*----END-IF.
     IF  NSI-F01  NOT =  "J02"
         GO TO  RD-INF-SEC
     END-IF.
*
*----MOVE  NSI-REC           TO  WSI-REC.
*
 RD-INF-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC              SECTION.
*
     MOVE  "MAIN-SEC"       TO  S-NAME.
*レコード作成
*    初期化
     MOVE   SPACE           TO  SIH-REC.
     INITIALIZE                 SIH-REC.
*
*    取引先ＣＤ
*----MOVE   WSI-F03         TO  SIH-F01.
     MOVE   137607          TO  SIH-F01.
*
*    検収日
*----MOVE   WSI-F05         TO  SIH-F02.
     MOVE   NSI-F25         TO  SIH-F02.
*
*    店舗ＣＤ
*----MOVE   WSI-F07         TO  SIH-F03.
     MOVE   NSI-F09         TO  SIH-F03.
*
*    伝票区分
*----MOVE   WSI-F08         TO  SIH-F04.
     MOVE   NSI-F22         TO  SIH-F04.
*
*    伝票番号
*----MOVE   WSI-F09         TO  SIH-F05.
*2013/03/21 NAV ST
*****MOVE   NSI-F23         TO  SIH-F05.
     MOVE   NSI-F23         TO  WK-NSI-F23.
     MOVE   WK-NSI-F23      TO  SIH-F05.
*2013/03/21 NAV ED
*
*    計上日
*----MOVE   WSI-F05         TO  SIH-F06.
     MOVE   NSI-F25         TO  SIH-F06.
*
*    支払金額１（税抜）
*----MOVE   WSI-F11         TO  SIH-F07.
*----IF     WSI-F10 = "-"
*----       COMPUTE SIH-F07 = WSI-F11 * -1
*----END-IF.
     MOVE   NSI-F332        TO  SIH-F07.
     IF     NSI-F331 = "-"
            COMPUTE SIH-F07 = NSI-F332 * -1
     END-IF.
*2012/11/08 NAV ST
     IF     NSI-F22 = 11 OR 12
            COMPUTE SIH-F07 = SIH-F07  * -1
     END-IF.
*2012/11/08 NAV ED
*
*    支払金額１（消費税）
     MOVE   NSI-F372        TO  SIH-F071.
     IF     NSI-F371 = "-"
            COMPUTE SIH-F071 = NSI-F372 * -1
     END-IF.
*2012/11/08 NAV ST
     IF     NSI-F22 = 11 OR 12
            COMPUTE SIH-F071 = SIH-F071 * -1
     END-IF.
*2012/11/08 NAV ED
*
*    支払金額１（税込）
     MOVE   NSI-F382        TO  SIH-F072.
     IF     NSI-F381 = "-"
            COMPUTE SIH-F072 = NSI-F382 * -1
     END-IF.
*2012/11/08 NAV ST
     IF     NSI-F22 = 11 OR 12
            COMPUTE SIH-F072 = SIH-F072 * -1
     END-IF.
*2012/11/08 NAV ED
*
*    支払金額２
*----MOVE   WSI-F13         TO  SIH-F08.
*----IF     WSI-F12 = "-"
*----       COMPUTE SIH-F08 = WSI-F13 * -1
*----END-IF.
     MOVE   ZERO            TO  SIH-F08.
*
*    部門
*    MOVE   WSI-F14         TO  SIH-F09.
     MOVE   NSI-F12         TO  SIH-F09.
*
*    相殺区分
*----MOVE   WSI-F15         TO  SIH-F10.
     MOVE   NSI-F27         TO  SIH-F10.
*
*    支払期間開始／終了
*----MOVE   WSH-F02         TO  SIH-F11.
*----MOVE   WSH-F03         TO  SIH-F12.
     MOVE   ZERO            TO  SIH-F11.
     MOVE   ZERO            TO  SIH-F12.
*
*    元伝票番号
     MOVE   NSI-F24         TO  SIH-F13.
*
*    理由区分
     MOVE   NSI-F42         TO  SIH-F14.
*
*    理由コード
     MOVE   NSI-F43         TO  SIH-F15.
*
*    前回保留ＦＬＧ
     MOVE   NSI-F46         TO  SIH-F16.
*
*    今回保留ＦＬＧ
     MOVE   NSI-F47         TO  SIH-F17.
*
* 2023/09/27 ADD START
*    VD値引消費税
     MOVE   NSI-F49         TO  SIH-F18.
*
*    標準税率当月相殺
     MOVE   NSI-F50         TO  SIH-F19.
*
*    標準相殺消費税
     MOVE   NSI-F51         TO  SIH-F20.
*
*    軽減税率当月相殺
     MOVE   NSI-F52         TO  SIH-F21.
*
*    軽減相殺消費税
     MOVE   NSI-F53         TO  SIH-F22.
*
*    登録番号
     MOVE   NSI-F54         TO  SIH-F23.
*
*    元取引日
     MOVE   NSI-F56         TO  SIH-F24.
* 2023/09/27 ADD END
*
     WRITE  SIH-REC.
     ADD    1               TO  OT-CNT.
*
     PERFORM  RD-INF-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.

     DISPLAY  "SIHANAF  IN  = "  IN-CNT  UPON CONS.
     DISPLAY  "SITGKF90 OUT = "  OT-CNT  UPON CONS.
* ファイルのＯＰＥＮ
     CLOSE  SIHANAF.
     CLOSE  SITGKF90.

 END-EXIT.
     EXIT.
*****************<<  SSI3701B   END PROGRAM  >>******************

```
