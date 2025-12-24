****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿              *
*    業務名　　　　　　　：　本社－営業所間在庫管理　　　　　　*
*    モジュール名　　　　：　（営業所）在庫差分更新            *
*    作成日／作成者　　　：　2000/05/20 TAKAHASHI              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SZA0040B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*商品在庫マスタ
     SELECT     ZAMZAIF       ASSIGN    TO       DA-01-VI-ZAMZAIF1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      ZAI-F01
                                                ZAI-F02
                                                ZAI-F03
                             FILE      STATUS   ZAI-ST.
*在庫差分ファイル
     SELECT     SABZAIKO     ASSIGN    TO       DA-01-S-SABZAIKO
                             FILE      STATUS   SAB-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*商品在庫マスタ
 FD  ZAMZAIF.
     COPY     ZAMZAIF    OF        XFDLIB
              JOINING   ZAI       PREFIX.
*在庫差分ファイル
 FD  SABZAIKO.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   SAB       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  ZAI-ST              PIC  X(02)  VALUE  SPACE.
     03  SAB-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  ZAMZAIF-INV-FLG      PIC  X(03)  VALUE  SPACE.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
     03  ADD-CNT             PIC  9(07)  VALUE  ZERO.
     03  UPDT-CNT            PIC  9(07)  VALUE  ZERO.
 01  WK-SYSTEM-DATE.
     03  SYS-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE-8          PIC  9(08)  VALUE  ZERO.
*商品在庫マスタ編集用
 01  WK-ZAI-REC.
     03  WK-ZAI-01           PIC  X(101).
     03  WK-ZAI-02           PIC  X(432).
     03  WK-ZAI-03           PIC  X(048).
     03  WK-ZAI-04           PIC  X(019).
*バックアップ商品在庫マスタ編集用
 01  WK-ZAIKO-REC.
     03  WK-ZAIKO-F01        PIC  X(101).
     03  WK-ZAIKO-F02        PIC  X(048).
     03  WK-ZAIKO-F03        PIC  X(051).
*
 01  FILE-ERR.
     03  ZAI-ERR             PIC  N(10)  VALUE
                   NC"在庫マスタ異常".
     03  SAB-ERR             PIC  N(10)  VALUE
                   NC"在庫差分Ｆ異常".
*
 01  SEC-NAME.
     03  FILLER              PIC  X(16)  VALUE "## ｴﾗｰSECTION = ".
     03  S-NAME              PIC  X(20).
     03  FILLER              PIC  X(03)  VALUE " ##".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     DISPLAY     ZAI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ZAI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 SAB-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SABZAIKO.
     DISPLAY     SAB-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     SAB-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL   END-FLG =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        INPUT       SABZAIKO.
     OPEN        I-O         ZAMZAIF.
*    差分在庫マスタ読込み
     PERFORM  SABZAIKO-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*商品在庫マスタ索引キーセット
     MOVE      SAB-F01      TO    ZAI-F01.
     MOVE      SAB-F02      TO    ZAI-F02.
     MOVE      SAB-F03      TO    ZAI-F03.
*商品在庫マスタ読込み
     PERFORM   ZAMZAIF-READ-SEC.
*更新判定
     IF        ZAMZAIF-INV-FLG = "INV"
               PERFORM ZAMZAIF-ADD-SEC
     ELSE
               PERFORM ZAMZAIF-UPDATE-SEC
     END-IF.
*    差分在庫マスタ読込み
     PERFORM  SABZAIKO-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*              在庫マスタ更新処理
****************************************************************
 ZAMZAIF-ADD-SEC              SECTION.
     MOVE     "ZAMZAIF-ADD-SEC"    TO   S-NAME.
*商品在庫マスタ初期化
     MOVE      SPACE      TO      ZAI-REC.
     INITIALIZE                   ZAI-REC.
*在庫差分ファイル項目セット
     MOVE      SAB-F01    TO      ZAI-F01.
     MOVE      SAB-F02    TO      ZAI-F02.
     MOVE      SAB-F03    TO      ZAI-F03.
     MOVE      SAB-F04    TO      ZAI-F04.
     MOVE      SAB-F05    TO      ZAI-F05.
     MOVE      SAB-F06    TO      ZAI-F06.
     MOVE      SAB-F07    TO      ZAI-F07.
     MOVE      SAB-F08    TO      ZAI-F08.
     MOVE      SAB-F09    TO      ZAI-F09.
     MOVE      SAB-F10    TO      ZAI-F10.
     MOVE      SAB-F11    TO      ZAI-F11.
     MOVE      SAB-F12    TO      ZAI-F12.
     MOVE      SAB-F13    TO      ZAI-F13.
     MOVE      SAB-F14    TO      ZAI-F14.
     MOVE      SAB-F15    TO      ZAI-F15.
     MOVE      SAB-F16    TO      ZAI-F16.
     MOVE      SAB-F17    TO      ZAI-F17.
     MOVE      SAB-F18    TO      ZAI-F18.
     MOVE      SAB-F19    TO      ZAI-F19.
     MOVE      SAB-F20    TO      ZAI-F20.
     MOVE      SAB-F21    TO      ZAI-F21.
     MOVE      SAB-F22    TO      ZAI-F22.
     MOVE      SAB-F23    TO      ZAI-F23.
     MOVE      SAB-F24    TO      ZAI-F24.
     MOVE      SAB-F25    TO      ZAI-F25.
     MOVE      SAB-F26    TO      ZAI-F26.
     MOVE      SAB-F27    TO      ZAI-F27.
     MOVE      SAB-F28    TO      ZAI-F28.
     MOVE      SAB-F29    TO      ZAI-F29.
     MOVE      SAB-F30    TO      ZAI-F30.
*レコード作成
     WRITE     ZAI-REC.
     ADD       1          TO      ADD-CNT.
*
 ZAMZAIF-ADD-EXIT.
     EXIT.
****************************************************************
*              在庫マスタ更新処理
****************************************************************
 ZAMZAIF-UPDATE-SEC           SECTION.
     MOVE     "ZAMZAIF-UPDATE-SEC" TO   S-NAME.
*    ＜現在庫数＞
     COMPUTE   ZAI-F04   =   ZAI-F04  +  SAB-F04.
*    ＜当月入出庫数＞
     COMPUTE   ZAI-F06   =   ZAI-F06  +  SAB-F06.
*    ＜当月発注数＞
     COMPUTE   ZAI-F20   =   ZAI-F20  +  SAB-F20.
*    ＜当月入庫数＞
     COMPUTE   ZAI-F07   =   ZAI-F07  +  SAB-F07.
*    ＜当月出庫数＞
     COMPUTE   ZAI-F08   =   ZAI-F08  +  SAB-F08.
*    ＜未入庫数＞
     COMPUTE   ZAI-F26   =   ZAI-F26  +  SAB-F26.
*    ＜未出庫数＞
     COMPUTE   ZAI-F27   =   ZAI-F27  +  SAB-F27.
     IF        ZAI-F27  <  ZERO
               MOVE      ZERO     TO     ZAI-F27
     END-IF.
*    ＜引当済数＞
     COMPUTE   ZAI-F28   =   ZAI-F28  +  SAB-F28.
     IF        ZAI-F28  <  ZERO
               MOVE      ZERO     TO     ZAI-F28
     END-IF.
*    ＜次月入庫数＞
     COMPUTE   ZAI-F11   =   ZAI-F11  +  SAB-F11.
*    ＜次月出庫数＞
     COMPUTE   ZAI-F12   =   ZAI-F12  +  SAB-F12.
*    ＜次月入出庫数＞
*****COMPUTE   ZAI-F19   =   ZAI-F19  +  SAB-F153.
*    ＜当月売上数＞
     COMPUTE   ZAI-F09   =   ZAI-F09  +  SAB-F09.
*    ＜次月売上数＞
     COMPUTE   ZAI-F13   =   ZAI-F13  +  SAB-F13.
*    ＜社内発注数＞
     COMPUTE   ZAI-F18   =   ZAI-F18  +  SAB-F18.
*    ＜社内発注消込数＞
     COMPUTE   ZAI-F19   =   ZAI-F19  +  SAB-F19.
*商品在庫マスタ更新
     REWRITE   ZAI-REC.
     ADD       1             TO          UPDT-CNT.
*
 ZAMZAIF-UPDATE-EXIT.
     EXIT.
****************************************************************
*            商品在庫マスタ読込み
****************************************************************
 ZAMZAIF-READ-SEC             SECTION.
     MOVE     "ZAMZAIF-READ-SEC"   TO   S-NAME.
*
     READ  ZAMZAIF
           INVALID
               MOVE    "INV"      TO   ZAMZAIF-INV-FLG
           NOT  INVALID
               MOVE    SPACE      TO   ZAMZAIF-INV-FLG
     END-READ.
*
 ZAMZAIF-READ-EXIT.
     EXIT.
****************************************************************
*            在庫差分ファイル読込み
****************************************************************
 SABZAIKO-READ-SEC           SECTION.
     MOVE     "SABZAIKO-READ-SEC" TO   S-NAME.
*
     READ  SABZAIKO  AT  END
               MOVE  "END"        TO   END-FLG
           NOT  AT  END
               ADD    1           TO   READ-CNT
     END-READ.
*
     IF        READ-CNT(6:2) = "00"
               DISPLAY "## READ-CNT = " READ-CNT " ##" UPON CONS
     END-IF.
*
 SABZAIKO-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE ZAMZAIF SABZAIKO.
*
     DISPLAY "## READ-CNT = " READ-CNT " ##"  UPON  CONS.
     DISPLAY "## ADD-CNT  = " ADD-CNT  " ##"  UPON  CONS.
     DISPLAY "## UPDT-CNT = " UPDT-CNT " ##"  UPON  CONS.
*
 END-EXIT.
     EXIT.
