****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿              *
*    業務名　　　　　　　：　振替依頼処理　　　　　　　　　　　*
*    モジュール名　　　　：　本社振替依頼データ削除            *
*    作成日／作成者　　　：　2000/06/20 Y.YOSHIDA              *
*    再利用ＰＧ　　　　　：                                    *
*    更新日／更新者　　　：　                                  *
*                            パラメタで受け取った倉庫コードと  *
*                            同一のデータを削除する            *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SFU0050B.
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
*振替依頼ファイル
     SELECT     FUIRAIF      ASSIGN    TO        DA-01-VI-FUIRAIL1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      SEQUENTIAL
                             RECORD    KEY       FIR-F01
                                                 FIR-F02
                                                 FIR-F03
                             FILE      STATUS    FIR-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*振替依頼マスタ
 FD  FUIRAIF.
     COPY     FUIRAIF    OF        XFDLIB
              JOINING    FIR       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  FIR-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  DEL-CNT             PIC  9(07)  VALUE  ZERO.
     03  READ-CNT            PIC  9(07)  VALUE  ZERO.
*
 01  FILE-ERR.
     03  FIR-ERR             PIC  N(10)  VALUE
                   NC"振替依頼ファイル異常".
*
 01  SEC-NAME.
     03  FILLER              PIC  X(16)  VALUE "## ｴﾗｰSECTION = ".
     03  S-NAME              PIC  X(20).
     03  FILLER              PIC  X(03)  VALUE " ##".
*
****************************************************************
 LINKAGE                     SECTION.
   01  LINK-SOKCD            PIC  X(02).
****************************************************************
 PROCEDURE                   DIVISION  USING     LINK-SOKCD.
****************************************************************
 DECLARATIVES.
 FIR-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE FUIRAIF.
     DISPLAY     FIR-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     FIR-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        I-O         FUIRAIF.
*
*振替依頼ファイル読込
     PERFORM   FUIRAI-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*
     IF        LINK-SOKCD    NOT =     SPACE
         IF    FIR-F01       =         LINK-SOKCD
               DELETE   FUIRAIF
               ADD      1    TO   DEL-CNT
         END-IF
     ELSE
               DELETE   FUIRAIF
               ADD      1    TO   DEL-CNT
     END-IF.
*
*振替依頼ファイル読込
     PERFORM   FUIRAI-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*              振替依頼ファイル読込
****************************************************************
 FUIRAI-READ-SEC             SECTION.
     MOVE     "FUIRAI-READ-SEC"   TO   S-NAME.
*
     READ     FUIRAIF   NEXT      AT END
              MOVE      9         TO   END-FLG
              GO        TO        FUIRAI-READ-EXIT
     END-READ.
*読込件数
     ADD      1    TO   READ-CNT.
*
 FUIRAI-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     CLOSE    FUIRAIF.
*
     DISPLAY "## READ-CNT   = " READ-CNT   " ##"  UPON  CONS.
     DISPLAY "## DEL-CNT    = " DEL-CNT    " ##"  UPON  CONS.
*
 END-EXIT.
     EXIT.
