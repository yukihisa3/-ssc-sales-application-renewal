***********************************************************
*                                                         *
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコＥＤＩ受信システム　         *
*    業務名　　　　　：ナフコＥＤＩ受信                   *
*    モジュール名　　：支払明細データ取込＆変換処理       *
*    作成日／更新日　：2010/10/19                         *
*    作成者／更新者　：ＮＡＶ阿部                         *
*    処理概要　　　　：                                   *
*      支払明細データを支払明細ファイルに変換し累積する。 *
*                                                         *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3771B.
 AUTHOR.               Y.A.
 DATE-WRITTEN.         2010/10/19.
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
     SELECT  NFSIHDT
       ASSIGN        TO  DA-01-S-NFSIHDT
       FILE STATUS   IS  SHD-ST.

*受領累積ファイル
     SELECT  NFSIHAF
       ASSIGN        TO  DA-01-VI-NFSIHAL1
       ORGANIZATION  IS  INDEXED
       ACCESS MODE   IS  RANDOM
       RECORD KEY    IS  SIH-F01 *> 店舗ＣＤ
                         SIH-F02 *> 部門ＣＤ
                         SIH-F03 *> 仕入先ＣＤ
                         SIH-F04 *> 締日
                         SIH-F05 *> 伝票区分
                         SIH-F06 *> 伝票番号
                         SIH-F07 *> 元伝票番号
                         SIH-F08 *> 受領日
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
 FD  NFSIHDT           BLOCK     CONTAINS  9    RECORDS
                       LABEL     RECORD    IS   STANDARD.
 01  SHD-REC.
     03  SHD-F01                PIC  X(03).
     03  FILLER                 PIC  X(409).

******************************************************************
*    支払明細ファイル
******************************************************************
 FD  NFSIHAF          LABEL     RECORD    IS   STANDARD.
                      COPY      NFSIHAF   OF   XFDLIB
                      JOINING   SIH       AS   PREFIX.

****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SHD-ST             PIC  X(02).
     03  SIH-ST             PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SHD-ERR            PIC N(15) VALUE
         NC"支払明細データエラー".
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
*
*支払明細編集レコード定義（明細）
 01  SHDW-REC.
     03  SHDW-F01               PIC  X(03).
     03  SHDW-F02               PIC  9(13).
     03  SHDW-F03               PIC  9(08).
     03  SHDW-F04               PIC  9(08).
     03  SHDW-F05               PIC  9(06).
     03  SHDW-F06               PIC  9(02).
     03  SHDW-F07S              PIC  X(01).
     03  SHDW-F07               PIC  N(10).
     03  SHDW-F07E              PIC  X(01).
     03  SHDW-F08               PIC  X(15).
     03  SHDW-F09               PIC  9(03).
     03  SHDW-F10S              PIC  X(01).
     03  SHDW-F10               PIC  N(10).
     03  SHDW-F10E              PIC  X(01).
     03  SHDW-F11               PIC  X(15).
     03  SHDW-F12               PIC  9(02).
     03  SHDW-F13S              PIC  X(01).
     03  SHDW-F13               PIC  N(10).
     03  SHDW-F13E              PIC  X(01).
     03  SHDW-F14               PIC  X(15).
     03  SHDW-F15               PIC  9(06).
     03  SHDW-F16S              PIC  X(01).
     03  SHDW-F16               PIC  N(10).
     03  SHDW-F16E              PIC  X(01).
     03  SHDW-F17               PIC  X(15).
     03  SHDW-F18               PIC  9(06).
     03  SHDW-F19S              PIC  X(01).
     03  SHDW-F19               PIC  N(10).
     03  SHDW-F19E              PIC  X(01).
     03  SHDW-F20               PIC  X(15).
     03  SHDW-F21               PIC  9(08).
     03  SHDW-F22               PIC  9(02).
     03  SHDW-F23               PIC  9(08).
     03  SHDW-F24               PIC  9(08).
     03  SHDW-F25               PIC  9(08).
     03  SHDW-F26               PIC  9(01).
     03  SHDW-F27               PIC  9(02).
     03  SHDW-F28               PIC  9(01).
     03  SHDW-F29               PIC  9(01).
     03  SHDW-F30               PIC  9(01).
     03  SHDW-F31               PIC  9(04).
     03  SHDW-F32               PIC  9(05).
     03  SHDW-F33X              PIC  X(01).
     03  SHDW-F33               PIC  9(08).
     03  SHDW-F34X              PIC  X(01).
     03  SHDW-F34               PIC  9(08).
     03  SHDW-F35X              PIC  X(01).
     03  SHDW-F35               PIC  9(08).
     03  SHDW-F36X              PIC  X(01).
     03  SHDW-F36               PIC  9(08).
     03  SHDW-F37X              PIC  X(01).
     03  SHDW-F37               PIC  9(08).
     03  SHDW-F38X              PIC  X(01).
     03  SHDW-F38               PIC  9(08).
     03  SHDW-F39X              PIC  X(01).
     03  SHDW-F39               PIC  9(08).
     03  SHDW-F40X              PIC  X(01).
     03  SHDW-F40               PIC  9(08).
     03  SHDW-F41X              PIC  X(01).
     03  SHDW-F41               PIC  9(08).
     03  SHDW-F42               PIC  9(02).
     03  SHDW-F43               PIC  9(02).
     03  SHDW-F44S              PIC  X(01).
     03  SHDW-F44               PIC  N(10).
     03  SHDW-F44E              PIC  X(01).
     03  SHDW-F45               PIC  9(01).
     03  SHDW-F46               PIC  9(01).
     03  SHDW-F47               PIC  9(01).
     03  SHDW-F48               PIC  9(01).
     03  FILLER                 PIC  X(391).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 SHD-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSIHDT.
     MOVE        SHD-ST     TO       E-ST.
     MOVE        "NFSIHDT" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SHD-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SIH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSIHAF.
     MOVE        SIH-ST    TO        E-ST.
     MOVE        "NFSIHAF" TO       E-FILE.
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
     OPEN  INPUT  NFSIHDT.
     OPEN  OUTPUT NFSIHAF.
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
     READ  NFSIHDT
       AT  END
         MOVE  "END"        TO  END-FLG
         GO TO  RD-INF-EXIT
     END-READ.

     ADD  1   TO  IN-CNT.

     IF  SHD-F01  = "JH2" *> ヘッダレコード
         GO TO  RD-INF-SEC
     END-IF.
*
     MOVE  SHD-REC           TO  SHDW-REC.
*
 RD-INF-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.
     PERFORM  EDWT-SEC.
     PERFORM  RD-INF-SEC.
 MAIN-EXIT.
     EXIT.
****************************************************************
*  編集/出力処理                                     2.1
****************************************************************
 EDWT-SEC               SECTION.
     MOVE  "EDWT-SEC"       TO  S-NAME.
*
     MOVE  SPACE             TO  SIH-REC.
     INITIALIZE                  SIH-REC.
*
     MOVE  SHDW-F09          TO  SIH-F01.
     MOVE  SHDW-F12          TO  SIH-F02.
     MOVE  SHDW-F18          TO  SIH-F03.
     MOVE  SHDW-F21          TO  SIH-F04.
     MOVE  SHDW-F22          TO  SIH-F05.
     MOVE  SHDW-F23          TO  SIH-F06.
     MOVE  SHDW-F24          TO  SIH-F07.
     MOVE  SHDW-F25          TO  SIH-F08.
     MOVE  SHDW-F26          TO  SIH-F09.
     MOVE  SHDW-F27          TO  SIH-F10.
     MOVE  SHDW-F31          TO  SIH-F11.
     MOVE  SHDW-F32          TO  SIH-F12.
*
     IF    SHDW-F33X  =  "-"
           COMPUTE   WK-KINGAKU  =     SHDW-F33  *  -1
           MOVE      WK-KINGAKU    TO  SIH-F13
     ELSE
           MOVE      SHDW-F33      TO  SIH-F13
     END-IF.
*
     IF    SHDW-F34X  =  "-"
           COMPUTE   WK-KINGAKU  =     SHDW-F34  *  -1
           MOVE      WK-KINGAKU    TO  SIH-F14
     ELSE
           MOVE      SHDW-F34      TO  SIH-F14
     END-IF.
*
     IF    SHDW-F35X  =  "-"
           COMPUTE   WK-KINGAKU  =     SHDW-F35  *  -1
           MOVE      WK-KINGAKU    TO  SIH-F15
     ELSE
           MOVE      SHDW-F35      TO  SIH-F15
     END-IF.
*
     IF    SHDW-F37X  =  "-"
           COMPUTE   WK-KINGAKU  =     SHDW-F37  *  -1
           MOVE      WK-KINGAKU    TO  SIH-F16
     ELSE
           MOVE      SHDW-F37      TO  SIH-F16
     END-IF.
*
     IF    SHDW-F38X  =  "-"
           COMPUTE   WK-KINGAKU  =     SHDW-F38  *  -1
           MOVE      WK-KINGAKU    TO  SIH-F17
     ELSE
           MOVE      SHDW-F38      TO  SIH-F17
     END-IF.
*
     IF    SHDW-F39X  =  "-"
           COMPUTE   WK-KINGAKU  =     SHDW-F39  *  -1
           MOVE      WK-KINGAKU    TO  SIH-F18
     ELSE
           MOVE      SHDW-F39      TO  SIH-F18
     END-IF.
*
     IF    SHDW-F40X  =  "-"
           COMPUTE   WK-KINGAKU  =     SHDW-F40  *  -1
           MOVE      WK-KINGAKU    TO  SIH-F19
     ELSE
           MOVE      SHDW-F40      TO  SIH-F19
     END-IF.
*
     IF    SHDW-F41X  =  "-"
           COMPUTE   WK-KINGAKU  =     SHDW-F41  *  -1
           MOVE      WK-KINGAKU    TO  SIH-F20
     ELSE
           MOVE      SHDW-F41      TO  SIH-F20
     END-IF.
*
     MOVE  SHDW-F42          TO  SIH-F21.
     MOVE  SHDW-F43          TO  SIH-F22.
     MOVE  SHDW-F07          TO  SIH-F23.
     MOVE  SHDW-F10          TO  SIH-F24.
     MOVE  SHDW-F13          TO  SIH-F25.
*
     WRITE  SIH-REC.
     ADD  1  TO  OT-CNT.
 EDWT-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.

     DISPLAY  "NFSIHDT IN = "  IN-CNT  UPON CONS.
     DISPLAY  "NFSIHAF OT = "  OT-CNT  UPON CONS.
* ファイルのＯＰＥＮ
     CLOSE  NFSIHDT.
     CLOSE  NFSIHAF.

 END-EXIT.
     EXIT.
*****************<<  SSY3771B   END PROGRAM  >>******************
