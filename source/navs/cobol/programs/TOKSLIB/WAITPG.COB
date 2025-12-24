****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　時間待ち受け処理                      *
*                        （待ち受け時間＝パラメタ）            *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            WAITPG.
 DATE-WRITTEN.          00/08/15.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS.
***************************************************************
 INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 WORKING-STORAGE           SECTION.
*--------------------------------------------------------------*
 01  TIMEX                   PIC  9(08).
 01  XWAITPG-TIME            PIC  9(06)  BINARY.
 01  XWAITPG-WORK            PIC  X(64).
*
 LINKAGE                   SECTION.
 01  PARA-TIME               PIC  9(06).
*
******************************************************************
 PROCEDURE                 DIVISION  USING  PARA-TIME.
******************************************************************
     ACCEPT       TIMEX      FROM        TIME.
     DISPLAY      TIMEX      UPON        CONS.
     MOVE         PARA-TIME  TO          XWAITPG-TIME.
     CALL         "XSBTWAIT" USING       XWAITPG-TIME
                                         XWAITPG-WORK.
     ACCEPT       TIMEX      FROM        TIME.
     DISPLAY      TIMEX      UPON        CONS.
     STOP         RUN.
*----------------------------------------------------------------*
